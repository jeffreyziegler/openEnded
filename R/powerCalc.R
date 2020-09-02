#' @title Power Calculation
#' 
#' @description 
#' Conduct a power calculation to make sure that you retain enough respondents to calculate a differential treatment effect if one likely exists.
#'  
#' @param model Model object from weighted regression that you wish to conduct a power analysis with.  
#' @param treatment_levels How many treatment conditions are there?
#' @param x_levels How many levels does the factor variable that is interacted with the treatment have?
#' @param alpha Default = 0.05
#' @param side Default = "two-sided"
#' 
#' @return 
#' 
#' @author Jeffrey Ziegler (<jeffrey.ziegler[at]emory.edu>)
#' 
#' @examples
#' powerCalc()
#' 
#' 
#' @rdname powerCalc
#' @export

powerCalc <- function(model, treatment_levels, x_levels, alpha = 0.05, side = "two-sided"){
    nTotal <- nobs(model)
    nPerCell <- floor(nTotal/(treatment_levels*x_levels))
    df1 <- (treatment_levels-1)*(x_levels-1)
    df2 <- treatment_levels*x_levels*(nPerCell-1)
    
    if(side=="two-sided"){
      F0 <- qf(p=1-alpha/2, df1=df1, df2=df2, ncp=0)
    }
    # null F stat (treatment effect for all groups = 0)
    F0 <- qf(p=1-alpha, df1=df1, df2=df2, ncp=0)
    # non-centrality parameter
    # see https://cran.r-project.org/web/packages/powerMediation/powerMediation.pdf
    ncp <- nPerCell*(mean(abs(model$coefficients))/sigma(model))^2
    # necessary power to detect alternate hypothesis if it exists
    power <- 1-pf(q=F0, df1=df1, df2=df2, ncp=ncp)
    # returns the probability that we reject the null 
    # (that there is no treatment effect among any of the groups) 
    # when it's actually false (there is a treatment effect among at least one of the groups)
    return(power)
  
}
