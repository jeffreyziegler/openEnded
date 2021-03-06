#' @title Estimate regression models for comparison
#' 
#' @description Compare the overall results varying on which respondents are included in the model based on attention.
#' 
#' @details 
#' This function generates three regressions with:
#' (1) the full sample irrespective of attention
#' (2) a subsetted sample based on list-wise deletion of respondents that "failed" manipulation check
#' (3) weighted sample by formula outlined in Ziegler (2020, 5) for average similarity.
#'
#' @param dataframe Dataframe from which you will select the outcome, predictors, and weights for the regression model.
#' @param formula Symbolic representation of the model to be estimated. This is written in "typical" R language (i.e. y ~ x1 + x2), such that y is the outcome variable and x1 and x2 are the predictors.
#' @param model_type Statistical model to estimate. Currently support OLS and logistic ("ls", "logit").
#' @param plot_treatment Character indicating vector in dataframe that's the treatment. Default is NULL, so users must specify for the function to work.
#' @param plot_interact_x Character indicating vector in dataframe that treatment should be plotted with. If there is an interaction in the formula, plot_interact_x is likely that variable. Default is NULL, so users must specify for the function to work.
#' @param correct_vec Character indicating vector in dataframe that indicates whether a respondent answered "correctly" as determined by a human coder. Default is NULL, so if a user doesn't include anything function will automatically set a threshold of "correctness" based on a respondent's average similarity (i.e. those respondents that score below say 0.1 will not be included in the "list-wise deletion" sample.
#' @param similarity_measures Vector(s) from dataframe that contains the similarity measures to be used as weights. 
#' @param k The penalty that you want to set for down-weighting inattentive respondents. Lower levels of k down-weight low attention participants more severely. 
#' @param up_down_weight Do you want to up-weight or down-weight?
#' @param user_seed Since we make random pulls from the multivariate normal distibution, set seed to get same results again. Default=5. 
#' @param n_sims Since we make random pulls from the multivariate normal distibution, set seed to get same results again. Default=10000. 
#' @param print_regs Return table of estimated regression coefficients and fit statistics formatted for LaTeX using texreg(). Default=FALSE.
#' @param plot_path If user wants to save figure, please provide a character vector for the file path in which the plot should be download. User must decide extension (pdf, jpg, png) in file path. 
#' @param stable_x Indicates whether plot_interact_x is continuous (stable_x=FALSE) or a factor (stable_x=TRUE). Currently the package only supports TRUE.
#' @param return_data Do you want the data that's used to construct the plot? Default = FALSE.
#' @param plotDifferences Do you want to see the marginal effects by model, or the differences between the models with regard to their marginal effects? Default=FALSE.

#' @return Three regression objects are estimated and loaded to your global environment as separate Zelig objects. They are named baseModel_"outcome variable", listwiseModel_"outcome variable", and weightedModel_"outcome variable".

#' @author Jeffrey Ziegler (<jeffrey.ziegler[at]emory.edu>)
#' 
#' @examples
#' regressionComparison(dataframe=kaneData, 
#'   formula=SelectTrump ~ NewsStoryConditions*ideologyFactor3, 
#'   plot_treatment="NewsStoryConditions",
#'   plot_interact_x="ideologyFactor3",
#'   similarity_measures=c("jaccardDist", "cosineDist"),
#'   k=3, 
#'   model_type="logit", 
#'   user_seed=5,  
#'   n_sims=10000, 
#'   correct_vec="correct", 
#'   display_plot=T,
#'   plot_path=NULL, 
#'   print_regs=T
#'   )
#'    
#' @seealso \code{\link{complierATE}} \code{\link{generateMarginalEffect}}
#' 
#' @rdname regressionComparison
#' @export

regressionComparison <- function(dataframe=NULL, 
                                 formula=NULL, 
                                 plot_treatment=NULL,
                                 plot_interact_x=NULL,
                                 similarity_measures=NULL,
                                 k=3, 
                                 model_type=NULL, 
                                 up_down_weight="down",
                                 user_seed=5,  
                                 n_sims=10000, 
                                 correct_vec=NULL, 
                                 display_plot=F,
                                 stable_x=T,
                                 plot_path=NULL, 
                                 print_regs=F,
                                 return_data=F,
                                 plotDifferences=F){
  # dplyr and tidy throw warning 
  # so we'll depress them for now and then turn them back on
  defaultW <- getOption("warn") 
  options(warn = -1) 
  
  # check basics of function by user
  if(is.null(dataframe) | is.null(formula)){
    error("You need to provide a dataframe and formula for the regression comparison.")
  }
  if(is.null(model_type)){
    error("You need to provide a family/model type (currently support OLS and logit).")
  }
  
  # calculate average weighted similarity measure
  if(is.null(similarity_measures)){
    dataframe <- averageSimilarity(dataframe, similarity_measures=c("jaccardSimilarity", "cosineSimilarity"), k, up_down_weight)
  }
  dataframe <- averageSimilarity(dataframe, similarity_measures, k, up_down_weight)
  
  # estimate models
  if(model_type=="ols"){
    base_model <- lm(formula, data=dataframe)
    weighted_model <- lm(formula, data=dataframe, weights=avgSimilarity)
    if(!is.null(correct_vec)){
      listwise_model <- lm(formula, data=dataframe[dataframe[[correct_vec]]==1,])
    }
    else{
      listwise_model <- lm(formula, data=dataframe[dataframe$avgSimilarity>.05,])
    }
  }
  # currently, only support logit aside from OLS
  if(model_type=="logit"){
    base_model <- glm(formula, data=dataframe, family=binomial(link = "logit"))
    set.seed(user_seed)
    weighted_model <- from_zelig_model(zelig(formula, 
                                             data=dataframe, 
                                             model="logit", 
                                             weights="avgSimilarity", 
                                             cite=F))
    message("Weights were set using Zelig.")
    if(!is.null(correct_vec)){
      listwise_model <- glm(formula, family=binomial(link = "logit"), data=dataframe[dataframe[[correct_vec]]==1,])
    } 
    else{
      listwise_model <- glm(formula, family=binomial(link = "logit"), data=dataframe[dataframe$avgSimilarity>.05,])
    }
  }
  # assign models to global environment
  # base models w/ all respondents
  assign(paste("baseModel", sub('\\. *', '', formula)[2], sep="_"),
         base_model, envir = .GlobalEnv)
  # removing inattentive participants
  assign(paste("listwiseModel", sub('\\. *', '', formula)[2], sep="_"),
         listwise_model, envir = .GlobalEnv)
  # re-weighting
  assign(paste("weightedModel", sub('\\. *', '', formula)[2], sep="_"),
         weighted_model, envir = .GlobalEnv)
  
  # check if user wants to print regressions tables for latex
  if(print_regs==T){
    # print output of regressions 
    print(texreg(list(base_model, listwise_model, weighted_model),
                 custom.model.names = c("Unweighted Model", "List-wise Deleted Model", "Weighted Model"),
                 digits=3, stars = c(0.001, 0.01, 0.05)))
  }
  # plot marginal effects
  # first, store the names of the variables used in the regression formula
  
  firstDiffPlotData <- list()
  temp_models <- list(base_model, listwise_model, weighted_model)
  for(sample in 1:length(temp_models)){
      # create model matrix of dummies from specified formula
      if(model_type=="ols"){
        full_dummies <- model.matrix(model.frame(formula, data=temp_models[[sample]]$model),
                                   data=temp_models[[sample]]$model)
      }
    
      if(model_type=="logit"){
        full_dummies <- model.matrix(model.frame(formula, data=temp_models[[sample]]$data),
                                   data=temp_models[[sample]]$data)
      }
      # get unique combos for sims of marginal effects
      unique_dummies <- unique(full_dummies)
      # point estimates from regression
      point_estimates <- temp_models[[sample]]$coefficients 
      # var-cov matrix
      var_cov_mat <- vcov(temp_models[[sample]])
      # simulate parameter distributions
      set.seed(user_seed)
      sim_betas <- as.matrix(mvrnorm(n_sims, point_estimates, var_cov_mat))
      if(model_type=="ols"){
        fd_labs <- unique(model.frame(formula, data=temp_models[[sample]]$model)[-1]) %>% unite("combo", sep=":", na.rm = T, remove = F)
      }
      if(model_type=="logit"){
        fd_labs <- unique(model.frame(formula, data=temp_models[[sample]]$data)[-1]) %>% unite("combo", sep=":", na.rm = T, remove = F)
      }
      # plot simulated first diffs
      if(plotDifferences!=T){
        firstDiffPlotData[[sample]] <- generateMarginalEffect(unique_covars = unique_dummies, 
                                                              simulated_betas=sim_betas, 
                                                              diff_labs=fd_labs[,1], 
                                                              model_type=model_type, 
                                                              plotDifferences=F)
        
        if(model_type=="ols"){
          
          firstDiffPlotData[[sample]]$treat_from <- sapply(str_match_all(firstDiffPlotData[[sample]]$covar_cats, 
                                                                         paste(unique(temp_models[[sample]]$model[, plot_treatment]),
                                                                               collapse = "|")), "[", 1)
          firstDiffPlotData[[sample]]$treat_to <- sapply(str_match_all(firstDiffPlotData[[sample]]$covar_cats, 
                                                                       paste(unique(temp_models[[sample]]$model[, plot_treatment]),
                                                                             collapse = "|")), "[", 2)
        }
        if(model_type=="logit"){
          
          firstDiffPlotData[[sample]]$treat_from <- sapply(str_match_all(firstDiffPlotData[[sample]]$covar_cats, 
                                                                         paste(unique(temp_models[[sample]]$data[, plot_treatment]),
                                                                               collapse = "|")), "[", 1)
          firstDiffPlotData[[sample]]$treat_to <- sapply(str_match_all(firstDiffPlotData[[sample]]$covar_cats, 
                                                                       paste(unique(temp_models[[sample]]$data[, plot_treatment]),
                                                                             collapse = "|")), "[", 2)
        }
        firstDiffPlotData[[sample]]$treat_from_to <- as.factor(paste(firstDiffPlotData[[sample]]$treat_from,
                                                                     firstDiffPlotData[[sample]]$treat_to,
                                                                     sep=" to "))
        
        firstDiffPlotData[[sample]]$interact_x_from <- sapply(str_match_all(firstDiffPlotData[[sample]]$covar_cats, 
                                                                            paste(temp_models[[sample]]$xlevels[[plot_interact_x]],
                                                                                  collapse = "|")), "[", 1) 
        firstDiffPlotData[[sample]]$interact_x_to <- sapply(str_match_all(firstDiffPlotData[[sample]]$covar_cats, 
                                                                          paste(temp_models[[sample]]$xlevels[[plot_interact_x]],
                                                                                collapse = "|")), "[", 2) 
        if(stable_x==T){
          firstDiffPlotData[[sample]] <- firstDiffPlotData[[sample]][which(firstDiffPlotData[[sample]]$interact_x_from==firstDiffPlotData[[sample]]$interact_x_to),]
          firstDiffPlotData[[sample]]$interact_x <- as.factor(firstDiffPlotData[[sample]]$interact_x_from)
          firstDiffPlotData[[sample]] <- firstDiffPlotData[[sample]][, c("first_diffs","lower_CI", "upper_CI", "treat_from_to", "interact_x")]
        }
      }
      if(plotDifferences==T){
        firstDiffPlotData[[sample]] <- generateMarginalEffect(unique_covars = unique_dummies, 
                                                              simulated_betas=sim_betas, 
                                                              diff_labs=fd_labs[,1], 
                                                              model_type=model_type, 
                                                              plotDifferences=T)
      }
      firstDiffPlotData[[sample]] <- firstDiffPlotData[[sample]][, sort(names(firstDiffPlotData[[sample]]))]
  }

  if(plotDifferences==T){
    browser()
    difference_bw_models <- NULL
    for(j in 1:(dim(firstDiffPlotData[[1]])[2])){
      difference_bw_models <- as.data.frame(rbind(difference_bw_models, cbind(as.data.frame(firstDiffPlotData[[1]])[, j]-as.data.frame(firstDiffPlotData[[2]])[, j], 
                                                                              as.data.frame(firstDiffPlotData[[1]])[, j]-as.data.frame(firstDiffPlotData[[3]])[, j],
                                                                              as.data.frame(firstDiffPlotData[[2]])[, j]-as.data.frame(firstDiffPlotData[[3]])[, j],
                                                                              j)))
      
    }
    difference_bw_models$j <- as.factor(difference_bw_models$j)
    levels(difference_bw_models$j) <- names(firstDiffPlotData[[1]])
    names(difference_bw_models) <- c("Full - Weighted", "Full - Listwise Deletion",
                                     "Weighted - Listwise Deletion", "treatment_effect")
    
    difference_bw_models$treat_from <- sapply(str_match_all(difference_bw_models$treatment_effect, 
                         paste(unique(temp_models[[sample]]$model[, plot_treatment]),
                               collapse = "|")), "[", 1)
    difference_bw_models$treat_to <- sapply(str_match_all(difference_bw_models$treatment_effect, 
                                      paste(unique(temp_models[[sample]]$model[, plot_treatment]),
                                            collapse = "|")), "[", 2)
    difference_bw_models$treat_from_to <- as.factor(paste(difference_bw_models$treat_from,
                                                                  difference_bw_models$treat_to,
                                                                 sep=" to "))
    
    difference_bw_models$interact_x_from <- sapply(str_match_all(difference_bw_models$treatment_effect, 
                                                                        paste(temp_models[[sample]]$xlevels[[plot_interact_x]],
                                                                              collapse = "|")), "[", 1) 
    difference_bw_models$interact_x_to <- sapply(str_match_all(difference_bw_models$treatment_effect, 
                                                                      paste(temp_models[[sample]]$xlevels[[plot_interact_x]],
                                                                            collapse = "|")), "[", 2) 
    

    
    difference_bw_models <- difference_bw_models[which(difference_bw_models$interact_x_from==difference_bw_models$interact_x_to),]
    difference_bw_models$interact_x <- as.factor(difference_bw_models$interact_x_from)
    
    #browser()
    
    difference_bw_modelsMelt <- melt(difference_bw_models, id = c("treat_from_to", "interact_x"),
                                     measure.vars = c("Full - Weighted", "Full - Listwise Deletion", "Weighted - Listwise Deletion"))

    difference_bw_modelsPlotData <- cbind(ddply(difference_bw_modelsMelt, .(treat_from_to, interact_x, variable), summarize, first_diffs = mean(value)),
                                          lower_CI = ddply(difference_bw_modelsMelt, .(treat_from_to, interact_x, variable), summarize, lower_CI = quantile(value, .025))[,4],
                                          upper_CI = ddply(difference_bw_modelsMelt, .(treat_from_to, interact_x,  variable), summarize, upper_CI = quantile(value, .975))[,4]
    )
    normal_data <- difference_bw_modelsPlotData %>% group_by(treat_from_to, variable) %>% summarise(n = n(), .groups = 'drop') %>% filter(n>1)
    weird_data <- difference_bw_modelsPlotData %>% group_by(treat_from_to, variable) %>% summarise(n = n(), .groups = 'drop') %>% filter(n==1)
    for(scenario in 1:dim(difference_bw_modelsPlotData)[1]){
      
      if(do.call(paste0, difference_bw_modelsPlotData[scenario, c("treat_from_to", "variable")])%in% do.call(paste0, weird_data[, c("treat_from_to", "variable")])){
        difference_bw_modelsPlotData[scenario, "first_diffs"] <- -1*difference_bw_modelsPlotData[scenario, "first_diffs"]
        difference_bw_modelsPlotData[scenario, "lower_CI"] <- -1*difference_bw_modelsPlotData[scenario, "lower_CI"] 
        difference_bw_modelsPlotData[scenario, "upper_CI"] <- -1*difference_bw_modelsPlotData[scenario, "upper_CI"]
        difference_bw_modelsPlotData[scenario, "treat_from_to"] <- paste(rev(strsplit(as.character(difference_bw_modelsPlotData[scenario, "treat_from_to"]), "\\s+")[[1]]), collapse= " ")
      }
      
      if(str_detect(difference_bw_modelsPlotData[scenario, c("treat_from_to")], 
                    levels(temp_models[[sample]]$model[, plot_treatment])[1]) & str_locate(difference_bw_modelsPlotData[scenario, c("treat_from_to")], 
                                                                                           levels(temp_models[[sample]]$model[, plot_treatment])[1])[1]!=1){
        difference_bw_modelsPlotData[scenario, "first_diffs"] <- -1*difference_bw_modelsPlotData[scenario, "first_diffs"]
        difference_bw_modelsPlotData[scenario, "lower_CI"] <- -1*difference_bw_modelsPlotData[scenario, "lower_CI"] 
        difference_bw_modelsPlotData[scenario, "upper_CI"] <- -1*difference_bw_modelsPlotData[scenario, "upper_CI"]
        difference_bw_modelsPlotData[scenario, "treat_from_to"] <- paste(rev(strsplit(as.character(difference_bw_modelsPlotData[scenario, "treat_from_to"]), "\\s+")[[1]]), collapse= " ")
      }
    }

    #pdf("figures/difference_bw_models(Kane).pdf", width=15, height=8)
    #browser()
    compareRegPlot <- ggplot(difference_bw_modelsPlotData) +
      #theme_classic() +# coord_flip()+
      theme_pubr() +
      # geom_pointrange(aes(x=variable, y = FDmean, ymin = FDlow90, ymax = FDhigh90, colour=subset),
      #                position = position_dodge(width =.75), size=1.5)+
      geom_pointrange(aes(x=treat_from_to, y = first_diffs, ymin = lower_CI, ymax = upper_CI, colour=variable, shape=variable),
                      position = position_dodge(width =.75), size=1.5)+
      scale_shape_manual(values = c(17,18,19, 16))+
      geom_hline(aes(yintercept= 0), linetype="dashed", size=.5, colour="black") +
      scale_colour_grey(start = 0, end = 0.7)+
      facet_wrap(~interact_x, ncol=3) + 
      theme(axis.title=element_text(size=20), axis.text = element_text(size=18), legend.text=element_text(size=18),
            strip.text = element_text(size=20), strip.background = element_rect(fill = NA, color = "black"),
            legend.position="bottom", legend.title = element_text(size=20),
            title = element_text(size=25),  legend.background = element_blank(),
            legend.box.background = element_rect(colour = "black"), axis.text.x = element_text(angle = 45, hjust = 1),
            panel.border = element_blank(),
            panel.background = element_rect(fill = NA, color = "black"),
            panel.grid = element_blank()
            #  strip.background = element_blank(),
            # strip.text.x = element_blank()
      ) +
      geom_vline(xintercept = c(1.5,2.5)) +
      labs(y='\nDifference Between Treatment Effects\n', x='\nTreatment Condition\n', colour="Sample\n(Model):", shape="Sample\n(Model):")

  }
  
  if(plotDifferences!=T){
    firstDiffPlotData <- bind_rows(firstDiffPlotData, .id = "column_label")
    
    firstDiffPlotData$column_label <- revalue(factor(firstDiffPlotData$column_label),
                                              c("1"="Full", 
                                                "2"="List-Wise Deletion", 
                                                "3"="Weighted"))
    
    normal_data <- firstDiffPlotData %>% group_by(treat_from_to, column_label) %>% summarise(n = n(), .groups = 'drop') %>% filter(n>1)
    weird_data <- firstDiffPlotData %>% group_by(treat_from_to, column_label) %>% summarise(n = n(), .groups = 'drop') %>% filter(n==1)
    for(scenario in 1:dim(firstDiffPlotData)[1]){
      # might need to do one for when k=1 and k=10
      # or when user_seed=1, 10, or 100, for some reason
      # the plots create an extra group
      
      if(do.call(paste0, firstDiffPlotData[scenario, c("treat_from_to", "column_label")])%in% do.call(paste0, weird_data[, c("treat_from_to", "column_label")])){
        firstDiffPlotData[scenario, "first_diffs"] <- -1*firstDiffPlotData[scenario, "first_diffs"]
        firstDiffPlotData[scenario, "lower_CI"] <- -1*firstDiffPlotData[scenario, "lower_CI"] 
        firstDiffPlotData[scenario, "upper_CI"] <- -1*firstDiffPlotData[scenario, "upper_CI"]
        firstDiffPlotData[scenario, "treat_from_to"] <- paste(rev(strsplit(as.character(firstDiffPlotData[scenario, "treat_from_to"]), "\\s+")[[1]]), collapse= " ")
      }
      
      if(str_detect(firstDiffPlotData[scenario, c("treat_from_to")], 
                    levels(temp_models[[sample]]$model[, plot_treatment])[1]) & str_locate(firstDiffPlotData[scenario, c("treat_from_to")], 
                                                                                           levels(temp_models[[sample]]$model[, plot_treatment])[1])[1]!=1){
        firstDiffPlotData[scenario, "first_diffs"] <- -1*firstDiffPlotData[scenario, "first_diffs"]
        firstDiffPlotData[scenario, "lower_CI"] <- -1*firstDiffPlotData[scenario, "lower_CI"] 
        firstDiffPlotData[scenario, "upper_CI"] <- -1*firstDiffPlotData[scenario, "upper_CI"]
        firstDiffPlotData[scenario, "treat_from_to"] <- paste(rev(strsplit(as.character(firstDiffPlotData[scenario, "treat_from_to"]), "\\s+")[[1]]), collapse= " ")
      }
    }
    
    # generate and temporarily save plot of treatment effects
    # by interact_x for base, weighted, and list-wise models
    compareRegPlot <- ggplot(firstDiffPlotData, aes(x=treat_from_to, y = first_diffs )) +
      theme_pubr() +
      geom_pointrange(aes(ymin = lower_CI, ymax = upper_CI, 
                          colour=column_label, shape=column_label
      ),
      position = position_dodge(width =.75), size=1.5)+
      scale_shape_manual(values = c(17,18,19, 16))+
      geom_hline(aes(yintercept= 0), linetype="dashed", size=.5, colour="black") +
      facet_wrap(~interact_x, ncol=3, scales = "fixed", drop = T) + 
      scale_colour_grey(start = 0, end = 0.7)+
      theme(axis.title=element_text(size=20), axis.text = element_text(size=18), 
            legend.text=element_text(size=18),
            strip.text = element_text(size=20), strip.background = element_rect(fill = NA, color = "black"),
            legend.position="bottom", legend.title = element_text(size=20),
            title = element_text(size=25),  legend.background = element_blank(),
            legend.box.background = element_rect(colour = "black"), axis.text.x = element_text(angle = 90, hjust = 1),
            panel.border = element_blank(),
            panel.background = element_rect(fill = NA, color = "black"),
            panel.grid = element_blank()
      ) +
      geom_vline(xintercept = c(1.5,2.5)) +
      labs(y='\nMarginal Effect of Treatment\n', x='\nTreatment\n', colour="Sample (Model):", shape="Sample (Model):")
  }
  
  # check to see if user wants to see plot
  if(display_plot==T){
    print(compareRegPlot)
  }
  
  # check to see if user wants to save plot
  if(!is.null(plot_path)){
      pdf(file=plot_path, width=11, height=7)
      print(compareRegPlot)
      dev.off()
      #error("Need to provide a path to save the plot")
  }
  if(return_data==TRUE){
    return(firstDiffPlotData)
  }
  # turn warnings back on
  options(warn = defaultW)
}

