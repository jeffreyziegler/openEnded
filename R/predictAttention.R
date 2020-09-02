#' @title Predict Attention Among Respondents
#' @description 
#' Estimate a linear regression in which attention is the outcome, and the user defines the characteristics used to predict attention.
#' 
#' @param dataframe 
#' @param formula Symbolic representation of the model to be estimated. This is written in "typical" R language (i.e. y ~ x1 + x2), such that y is the outcome variable (attention) and x_1,..., x_J are the predictors.

#' @return 
#'
#' @author Jeffrey Ziegler (<jeffrey.ziegler[at]emory.edu>)
#' @examples

#' @rdname predictAttention
#' @export

predictAttention <- function(dataframe=NULL, 
                        formula=NULL, 
                        plot_treatment=NULL,
                        plot_interact_x=NULL,
                        similarity_measures=c("jaccardDist", "cosineDist"),
                        bounds=c(0.1, 0.2), 
                        n=100, 
                        user_seed=5, 
                        model_type=NULL, 
                        k=3,
                        display_plot=T,
                        plot_path=NULL,
                        stable_x){
  
  
}
