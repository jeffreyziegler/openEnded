#' @title executeMarginalEffect
#' @description 
#' Calculate the overall treatment effects differing how inattentive participants are down-weighted.
#' 
#' @param dataframe Dataframe that contains the prompt and response of the open-ended manipulation check.
#' @param regression_models Set of regression models that you wish to estimate the marginal effects for. In the example, there are five outcomes,  which appears in the example below.
#'
#' @return Dataframe of all estimated marginal effects that make up the sampling distribution.
#'
#' @author Jeffrey Ziegler (<jeffrey.ziegler[at]emory.edu>)
#' @examples
#' executeMarginalEffect(subset="full", regression_model=baseModel_trustChurch_postTreat)
#' 
#' @seealso  \code{\link{regressionComparison}} \code{\link{generateMarginalEffect}} \code{\link{plotMarginalEffect}} \code{\link{plotComplierATE}}
#' 
#' @rdname executeMarginalEffect
#' @export

executeMarginalEffect <- function(subset=NULL, regression_model){
    if(!any(grepl(subset, c("weighted", "full", "listwise")))){
      stop('Please tell me how the data is subsetted! Your options:\n"weighted", "full", or "listwise"')
    }
  browser()
  marginal_data <- generateMarginalEffect(regression_model)
  marginal_data$subset <- subset
  marginal_data$outcome <- sub('\\. *', '', regression_model$formula)[2]
  return(marginal_data)
}
