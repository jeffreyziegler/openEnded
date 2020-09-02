#' @title Estimate Instrumental Variable Model for LATE
#' 
#' @description Estimate the LATE using an instrumental variable approach to compare with the simulated sampling distribution of participants that likely received and didn't receive the treatment.
#' 
#' @details 
#' This function estimate an IV regression and generate a regression table of results with LaTeX formatting.
#'
#' @param dataframe Dataframe from which you will select the outcome, predictors, and weights for the regression model.
#' @param formula Symbolic representation of the model to be estimated. This is written in "typical" R language (i.e. y ~ x1 + x2), such that y is the outcome variable and x_1,..., x_J are the predictors.
#' @param similarity_measures Vector(s) from dataframe that contains the similarity measures to be used as weights. 
#' @param k The penalty that you want to set for down-weighting inattentive respondents. Lower levels of k down-weight low attention participants more severely. 
#' @param up_down_weight Do you want to up-weight or down-weight?

#' @param print_regs Return table of estimated regression coefficients and fit statistics formatted for LaTeX using texreg(). Default=FALSE.

#' @return 

#' @author Jeffrey Ziegler (<jeffrey.ziegler[at]emory.edu>)
#' 
#' @examples
#' 
#'    
#' @seealso \code{\link{complierATE}} \code{\link{regressionComparison}}
#' 
#' @rdname IVcomparison
#' @export

IVcomparison <- function(dataframe=NULL, 
                         formula=NULL, 
                         similarity_measures=NULL,
                         k=3, 
                         up_down_weight="up",
                         print_regs=F){
  
  # check basics of function by user
  if(is.null(dataframe) | is.null(formula)){
    error("You need to provide a dataframe and formula for the regression comparison.")
  }
  if(is.null(formula)){
    error("You need to provide a formula for the regression you want to be estimated.")
  }
  
  # calculate average weighted similarity measure
  if(is.null(similarity_measures)){
    dataframe <- averageSimilarity(dataframe, similarity_measures=c("jaccardSimilarity", "cosineSimilarity"), k, up_down_weight)
  }
  
  dataframe <- averageSimilarity(dataframe, similarity_measures, k, up_down_weight)
  
  # estimate iv regression
  iv_out <- ivreg(paste(as.character(formula)[2], "~", as.character(formula)[3], "|",
                        as.character(formula)[3], "+ avgSimilarity"), data = dataframe)
  

  # assign models to global environment
  assign(paste("ivModel", sub('\\. *', '', formula)[2], sep="_"),
         iv_out, envir = .GlobalEnv)
  
  # check if user wants to print regressions tables for latex
  if(print_regs==T){
    # print output of regressions 
    print(texreg(list(iv_out),
                 digits=3, stars = c(0.001, 0.01, 0.05)))
  }
  
}

