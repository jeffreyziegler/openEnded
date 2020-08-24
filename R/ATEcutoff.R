#' @title Create ATE Cutoff
#' @description 
#' Create cutoff, then bootstrap or simulate the sampling distribution of participants that likely received the treatment (ATE among "compliers" and "non-compliers").
#' 
#' @param dataframe Dataframe from which we will estimate our regression model.
#' @param similarity_measures Vector(s) from dataframe that contains the similarity measures to be used as weights. Possible values for measure_type = c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex"). Default is "jaccard".
#' @param formula Symbolic representation of the model to be estimated. This is written in "typical" R language (i.e. y ~ x1 + x2), such that y is the outcome variable and x1 and x2 are the predictors.
#' @param k The penalty that you want to set for down-weighting inattentive respondents. Lower levels of k down-weight low attention participants more severely. 
#' @param model_type Statistical model to estimate. Currently support OLS and logistic ("ls", "logit").
#' @param bounds Minimum and maximum of uniform distribution we should draw cutoff values between.
#' 
#' @return Dataframe of all estimated marginal effects that make up the sampling distribution for participants that did and did not receive the treatment.
#' 
#' @author Jeffrey Ziegler (<jeffrey.ziegler[at]emory.edu>)
#' @examples
#' ATEcutoff(dataframe=replication_complete.cases, similarity_measures=c("jaccardDist", "cosineDist"), bounds=0.2, formula=trustChurch_postTreat ~ Concordant*attendanceBin, model_type="ls", k=3))
#' 
#' @seealso \code{\link{plotComplierATE}}
#' 
#' @rdname ATEcutoff
#' @export

ATEcutoff <- function(dataframe, 
                      formula, 
                      similarity_measures, 
                      model_type, 
                      k, 
                      bounds, 
                      user_seed,
                      plot_treatment=NULL,
                      plot_interact_x=NULL, 
                      stable_x){
    #browser()
    cutoff <- runif(1, min=bounds[1], max=bounds[2])
    dataframe <- averageSimilarity(dataframe, similarity_measures,  k)
    dataframe$passFail <- ifelse(dataframe[, "avgSimilarity"] <= cutoff, 0, 1)
    
    # estimate models
    if(model_type=="ols"){
        passReg <- lm(formula, data=dataframe[which(dataframe$passFail==1),])
        failReg <- lm(formula, data=dataframe[which(dataframe$passFail==0),])
        
    }
    # currently, only support logit aside from OLS
    if(model_type=="logit"){
        passReg <- glm(formula, data=dataframe[which(dataframe$passFail==1),], family=binomial(link="logit"))
        failReg <- glm(formula, data=dataframe[which(dataframe$passFail==0),], family=binomial(link="logit"))
    }
    
    pass1stDiffPlotData <- list()
    temp_models <- list(passReg, failReg)
    for(sample in 1:length(temp_models)){
        # create model matrix of dummies from specified formula
        
        full_dummies <- model.matrix(model.frame(formula, data=temp_models[[sample]]$data),
                                     data=temp_models[[sample]]$data)
        # get unique combos for sims of marginal effects
        unique_dummies <- unique(full_dummies)
        # point estimates from regression
        point_estimates <- temp_models[[sample]]$coefficients 
        # var-cov matrix
        var_cov_mat <- vcov(temp_models[[sample]])
        # simulate parameter distributions
        sim_betas <- as.matrix(mvrnorm(10000, point_estimates, var_cov_mat))
       
        fd_labs <- unique(model.frame(formula, data=temp_models[[sample]]$data)[-1]) %>% unite("combo", sep=":", na.rm = T, remove = F)
        # plot simulated first diffs
        pass1stDiffPlotData[[sample]] <- generateMarginalEffect(unique_covars = unique_dummies, 
                                                                simulated_betas=sim_betas, 
                                                                diff_labs=fd_labs[,1],
                                                                raw=F)
        #browser()
        
        #pass1stDiffPlotData[[sample]] <- reshape2::melt(pass1stDiffPlotData[[sample]])
        #grep(paste(toMatch,collapse="|"), 
        #     myfile$Letter, value=TRUE)
        pass1stDiffPlotData[[sample]]$treat_from <- sapply(str_match_all(pass1stDiffPlotData[[sample]]$covar_cats, 
                                                                         paste(unique(temp_models[[sample]]$data[, plot_treatment]),
                                                                               collapse = "|")), "[", 1)
        pass1stDiffPlotData[[sample]]$treat_to <- sapply(str_match_all(pass1stDiffPlotData[[sample]]$covar_cats, 
                                                                       paste(unique(temp_models[[sample]]$data[, plot_treatment]),
                                                                             collapse = "|")), "[", 2)
        
        pass1stDiffPlotData[[sample]]$treat_from_to <- as.factor(paste(pass1stDiffPlotData[[sample]]$treat_from,
                                                                       pass1stDiffPlotData[[sample]]$treat_to,
                                                                       sep=" to "))
        
        pass1stDiffPlotData[[sample]]$interact_x_from <- sapply(str_match_all(pass1stDiffPlotData[[sample]]$covar_cats, 
                                                                              paste(temp_models[[sample]]$xlevels[[plot_interact_x]],
                                                                                    collapse = "|")), "[", 1) 
        pass1stDiffPlotData[[sample]]$interact_x_to <- sapply(str_match_all(pass1stDiffPlotData[[sample]]$covar_cats, 
                                                                            paste(temp_models[[sample]]$xlevels[[plot_interact_x]],
                                                                                  collapse = "|")), "[", 2) 
        if(stable_x==T){
            pass1stDiffPlotData[[sample]] <- pass1stDiffPlotData[[sample]][which(pass1stDiffPlotData[[sample]]$interact_x_from==pass1stDiffPlotData[[sample]]$interact_x_to),]
            pass1stDiffPlotData[[sample]]$interact_x <- as.factor(pass1stDiffPlotData[[sample]]$interact_x_from)
            pass1stDiffPlotData[[sample]] <- pass1stDiffPlotData[[sample]][, c("first_diffs", "treat_from_to", "interact_x")]
        }
        
    }
    pass1stDiffPlotData <- bind_rows(pass1stDiffPlotData, .id = "column_label")
    
    pass1stDiffPlotData$subset <- c(rep("Compliers", dim(pass1stDiffPlotData)[1]/2), rep("Non-Compliers",  dim(pass1stDiffPlotData)[1]/2))
    normal_data <- pass1stDiffPlotData %>% group_by(treat_from_to, interact_x) %>% summarise(n = n(), .groups = 'drop') %>% filter(n>10000)
    weird_data <- pass1stDiffPlotData %>% group_by(treat_from_to, interact_x) %>% summarise(n = n(), .groups = 'drop') %>% filter(n==10000)
    
    for(scenario in 1:dim(pass1stDiffPlotData)[1]){
        # might need to do one for when k=1 and k=10
        # or when user_seed=1, 10, or 100, for some reason
        # the plots create an extra group
       #browser()
        
        if(do.call(paste0, pass1stDiffPlotData[scenario, c("treat_from_to", "interact_x")])%in% do.call(paste0, weird_data[, c("treat_from_to", "interact_x")])){
            pass1stDiffPlotData[scenario, "first_diffs"] <- -1*pass1stDiffPlotData[scenario, "first_diffs"]
            pass1stDiffPlotData[scenario, "treat_from_to"] <- paste(rev(strsplit(as.character(pass1stDiffPlotData[scenario, "treat_from_to"]), "\\s+")[[1]]), collapse= " ")
        }
        
        if(str_detect(pass1stDiffPlotData[scenario, c("treat_from_to")], "Control") & str_locate(pass1stDiffPlotData[scenario, c("treat_from_to")], "Control")[1]!=1){
            pass1stDiffPlotData[scenario, "first_diffs"] <- -1*pass1stDiffPlotData[scenario, "first_diffs"]
            pass1stDiffPlotData[scenario, "treat_from_to"] <- paste(rev(strsplit(as.character(pass1stDiffPlotData[scenario, "treat_from_to"]), "\\s+")[[1]]), collapse= " ")
        }
        
        if(str_detect(pass1stDiffPlotData[scenario, c("treat_from_to")], "Appease to Alienate")){
            pass1stDiffPlotData[scenario, "first_diffs"] <- -1*pass1stDiffPlotData[scenario, "first_diffs"]
            pass1stDiffPlotData[scenario, "treat_from_to"] <- paste(rev(strsplit(as.character(pass1stDiffPlotData[scenario, "treat_from_to"]), "\\s+")[[1]]), collapse= " ")
        }
    }
        #pass1stDiffPlotData$outcome <- str_to_title(gsub("_.*","", gsub('([[:upper:]])', ' \\1', gsub("~.*","", formula)[2])))
        pass1stDiffPlotData$cutoff <- cutoff
        #browser()
        
        return(pass1stDiffPlotData) 
}
