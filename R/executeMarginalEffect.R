#' @title Execute Marginal Effect
#' @description 
#' Run the essential generateMarginalEffects function to calculate the overall treatment effects differing how inattentive participants are down-weighted.
#' 
#' @param subset Which sample is used to estimate this regression model? Full ("base"), listwise deleting those that incorrectly answer manipulation check ("listwise"), or weighted using WLS based on continuous measure of attention ("weighted").
#' @param regression_model Regression object indicating one model that you wish to calculate the marginal effects for.
#'
#' @return Dataframe of all estimated marginal effects that make up the sampling distribution. If used with plotMarginalEffect, you will automatically receive the sampling distributions for all three types of subsets to compare against one another.
#'
#' @author Jeffrey Ziegler (<jeffrey.ziegler[at]emory.edu>)
#' @examples
#' executeMarginalEffect(subset="base", regression_model=baseModel_trustChurch_postTreat)
#' 
#' @seealso  \code{\link{regressionComparison}} \code{\link{generateMarginalEffect}} \code{\link{plotMarginalEffect}} \code{\link{plotComplierATE}}
#' 
#' @rdname executeMarginalEffect
#' @export

executeMarginalEffect <- function(subset=NULL, regression_model=NULL){
  if(!any(grepl(subset, c("weighted", "base", "listwise")))){
    stop('Please tell me how the data is subsetted! Your options:\n"weighted", "base", or "listwise"')
  }
  if(is.null(regression_model)){
    stop('Please provide a regression model.')
  }
  marginal_data <- as.data.frame(generateMarginalEffect(regression_model))
  marginal_data$subset <- subset
  marginal_data$outcome <- sub('\\. *', '', regression_model$formula)[2]
  return(marginal_data)
}
