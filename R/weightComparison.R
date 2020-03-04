#' @title Compare the overall results varying on which respondents are included in the model based on attention
#' @description
#' 
#' This function generates three regressions with:
#' (1) the full sample irrespective of attention
#' (2) a subsetted sample based on list-wise deletion of respondents that "failed" manipulation check
#' (3) weighted sample by forumal for average similarity 
#'
#' @param dataframe Dataframe from which you will select the outcome, predictors, and weights for the regression model.
#'
#' @return three regression objects

#' @author Jeffrey Ziegler
#' @examples
#' 
#' \dontrun{
#' regressionComparison()
#' }
#' @rdname regressionComparison
#' @export

regressionComparison <- function(dataframe, formula, model_type){

  # base models w/ all respondents
  baseLM <- zelig(formula, data=dataframe, model=paste(model_type))

  # re-weighting
  baseLMweights <- zelig(SelectTrump ~ NewsStoryConditions*ideologyFactor3, data=kaneData, model="logit", weights="avgDist")
  
  # removing inattentive participants
  baseLMCorrectSubset <- zelig(SelectTrump ~ NewsStoryConditions*ideologyFactor3, data=kaneData[kaneData$correct==1,], model="logit")
  
  texreg(list(baseLM, baseLMweights, baseLMDistanceSubset, baseLMCorrectSubset),digits = 3, stars = c(0.01, 0.05, 0.1))
}
