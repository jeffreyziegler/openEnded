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
#' @param similarity_measures Vector(s) from dataframe that contains the similarity measures to be used as weights. 
#' @param k The penalty that you want to set for down-weighting inattentive respondents. Lower levels of k down-weight low attention participants more severely. 
#' @param model_type Statistical model to estimate. Currently support OLS and logistic ("ls", "logit").
#' 
#' @return Three regression objects are estimated and loaded to your global environment as separate Zelig objects. They are named baseModel, listwiseModel, and weightedModel.

#' @author Jeffrey Ziegler (<jeffrey.ziegler[at]emory.edu>)
#' 
#' @examples
#' regressionComparison(formula=trustChurch_postTreat ~ Concordant*attendanceBin, dataframe=replication_complete.cases, k=3, similarity_measures=c("jaccardDist", "cosineDist"), model_type="ls")
#' 
#' @seealso \code{\link{plotMarginalEffect}} \code{\link{plotComplierATE}}
#' 
#' @rdname regressionComparison
#' @export

regressionComparison <- function(dataframe, formula, similarity_measures, k, model_type){
  # create empty vector to fill with each component of the "average" similarity
  additive_similarity <- NULL
  # loop over all similarity measures user specified in similarity_measures
  for(measure in similarity_measures){
    # take 1-each measure times their proportion to the average
    additive_similarity <- cbind(additive_similarity, (1-dataframe[, measure])*(1/length(similarity_measures)))
  }
  # sum all of the similarities up and take 1 - mean(1-similarity)^k
  # and add column of average similarity to original dataframe
  dataframe[, "avgSimilarity"] <- 1 - (rowSums(additive_similarity)^k)
  #browser()
  # base models w/ all respondents
  assign(paste("baseModel", sub('\\. *', '', formula)[2], sep="_"), 
         zelig(formula, data=dataframe, model=model_type, cite=F), envir = .GlobalEnv)
  
  # removing inattentive participants
  assign(paste("listwiseModel", sub('\\. *', '', formula)[2], sep="_"),
         zelig(formula, model=model_type,data=dataframe[dataframe$avgSimilarity>.1,], cite=F), envir = .GlobalEnv)
  
  # re-weighting
  assign(paste("weightedModel", sub('\\. *', '', formula)[2], sep="_"),
         zelig(formula, data=dataframe, model=model_type, weights="avgSimilarity", cite=F), 
         envir = .GlobalEnv)
  
}
