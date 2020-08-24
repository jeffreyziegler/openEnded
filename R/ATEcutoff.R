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

ATEcutoff <- function(formula, n_sims, similarity_measures, model_type, k=3, bounds=c(0.1, 0.2)){
    cutoff <- 1-runif(1, min=bounds[1], max=bounds[2])
    dataframe <- averageSimilarity(dataframe, similarity_measures,  k)
    dataframe$passFail <- ifelse(dataframe[, "avgSimilarity"] >= cutoff, 0, 1)

    passReg <- zelig(formula, data=dataframe[which(dataframe$passFail==1),], model=model_type, weights="avgSimilarity", cite=F)
    failReg <- zelig(formula, data=dataframe[which(dataframe$passFail==0),],  model=model_type, weights="avgSimilarity", cite=F)
    
    pass1stDiffPlotData <- NULL
    pass1stDiffPlotData <- rbind(generateMarginalEffect(temp_model = passReg),
                                 generateMarginalEffect(temp_model = failReg))
    
    pass1stDiffPlotData$subset <- c(rep("pass", n_sims), rep("fail", n_sims))
    
    pass1stDiffPlotData$outcome <- str_to_title(gsub("_.*","", gsub('([[:upper:]])', ' \\1', gsub("~.*","", formula)[2])))
    pass1stDiffPlotData$cutoff <- cutoff
    return(pass1stDiffPlotData) 
}
