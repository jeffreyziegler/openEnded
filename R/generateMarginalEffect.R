#' @title Generate Marginal Effects
#' 
#' @description 
#' This is an internal function that helps calculate the overall treatment effects differing how inattentive participants are down-weighted.
#' 
#' @param temp_model Regression object indicating one model that you wish to calculate the marginal effects for.
#'
#' @return Dataframe of marginal effects.
#'
#' @author Jeffrey Ziegler (<jeffrey.ziegler[at]emory.edu>)
#' @examples
#' generateMarginalEffect(baseModel_trustChurch_postTreat)
#' 
#' @seealso  \code{\link{regressionComparison}} \code{\link{executeMarginalEffect}} \code{\link{plotMarginalEffect}} \code{\link{plotComplierATE}}
#' 
#' @rdname generateMarginalEffect
#' @export

generateMarginalEffect <- function(temp_model){
    # store the names of the variables used in the regression formula
    formula_vars <- as.character(attr(terms(temp_model$formula), "variables"))
    # skip past outcome and "list()"

    covariate_data <- temp_model$data[, formula_vars[3:length(formula_vars)]]
    # create list of unique elements for each covariate
    # to iterate over for to estimate the marginal effect for all
    # combinations of covariate levels
    unique_elements <- NULL
    for(covar in 1:ncol(covariate_data)){
      unique_elements <- c(unique(na.omit(covariate_data[, covar])), unique_elements)
      # take only those unique elements of unique_elements
      unique_elements <- unique(unique_elements)
    }

    # flip list around so that the "first" covariate 
    # comes first in the list
    unique_elements <- unique_elements[rev.default(seq_along(unique_elements))]
    # so in the example, 'Concordant' comes first
    
    # execute the setx function for all combos of covariates
    fd <- NULL

    for(j in unique_elements[[2]]){
      setx_elements <- NULL
      for(k in unique_elements[[1]]){
      setx_elements <- c(setx(temp_model, Concordant=k, #names(covariate_data)[2]==j, 
                              attendanceBin=j), #names(covariate_data)[1]==k),
                              setx_elements)
      }
      fd <- cbind(fd, unlist(sim(temp_model, x = setx_elements[[2]], x1 = setx_elements[[1]], num = 10000)$sim.out[["x1"]][["fd"]]))    
    }

    fd <- as.data.frame(fd)
    names(fd) <- c("Never/yearly (Non-responsive to Responsive)", 
                    "Weekly (Non-responsive to Responsive)",
                    "Monthly (Non-responsive to Responsive)")
    return(fd)
}   
