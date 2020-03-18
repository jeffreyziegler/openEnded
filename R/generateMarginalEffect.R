#' @title generateMarginalEffect
#' @description 
#' Calculate the overall treatment effects differing how inattentive participants are down-weighted.
#' 
#' @param dataframe Dataframe that contains the prompt and response of the open-ended manipulation check.
#' @param 
#'
#' @return Vector of similarity measures (same length as input dataframe)
#'
#' @author Jeffrey Ziegler (<jeffrey.ziegler[at]emory.edu>)
#' @examples
#' generateMarginalEffect(temp_model)
#' 
#' @seealso  \code{\link{regressionComparison}} \code{\link{executeMarginalEffect}} \code{\link{plotMarginalEffect}} \code{\link{plotComplierATE}}
#' 
#' @rdname generateMarginalEffect
#' @export

generateMarginalEffect <- function(temp_model){
  browser()
    a10 <-  setx(temp_model, Concordant=0, attendanceBin="Never/yearly")
    a11 <-  setx(temp_model, Concordant=1, attendanceBin="Never/yearly")
    
    a20 <-  setx(temp_model, Concordant=0, attendanceBin="Weekly")
    a21 <-  setx(temp_model, Concordant=1, attendanceBin="Weekly")
    
    a30 <-  setx(temp_model, Concordant=0, attendanceBin="Monthly")
    a31 <-  setx(temp_model, Concordant=1, attendanceBin="Monthly")
    
    ss11 <- sim(temp_model, x = a10, x1 = a11, num = 10000)
    
    ss21 <- sim(temp_model, x = a20, x1 = a21, num = 10000)
    
    ss31 <- sim(temp_model, x = a30, x1 = a31, num = 10000)
    
    fd <- as.data.frame(cbind(unlist(ss11$sim.out[["x1"]][["fd"]]),
                              # unlist(ss12$sim.out[["x1"]][["fd"]]),
                              
                              unlist(ss21$sim.out[["x1"]][["fd"]]),
                              #unlist(ss22$sim.out[["x1"]][["fd"]]),
                              
                              unlist(ss31$sim.out[["x1"]][["fd"]])#,
                              # unlist(ss32$sim.out[["x1"]][["fd"]]),
    ))
    names(fd) <- c("Never/yearly (Non-responsive to Responsive)", 
                   "Weekly (Non-responsive to Responsive)",
                   "Monthly (Non-responsive to Responsive)")
    
    #outputMat <- rbind(outputMat, fd)
    return(fd)
}
