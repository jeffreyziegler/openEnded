#' @title Predict Attention Among Respondents
#' @description 
#' Estimate a linear regression in which attention is the outcome, and the user defines the characteristics used to predict attention.
#' 
#' @param dataframe Dataframe from which you will select attention, predictors for attention, etc. for the regression model.
#' @param attention_predictors Symbolic representation of the model to be estimated. This is written in "typical" R language (i.e. x1 + x2). Do not need to specify y or outcome variable, it's assigned internally.
#' @param k The penalty that you want to set for down-weighting inattentive respondents. Lower levels of k down-weight low attention participants more severely. 
#' @param up_down_weight Do you want to up-weight or down-weight? Default="down".

#' @return Object containing instrumental variable regression call, data, results, etc.
#'
#' @author Jeffrey Ziegler (<jeffrey.ziegler[at]emory.edu>)

#' @rdname predictAttention
#' @export

predictAttention <- function(dataframe=NULL, 
                             attention_formula=NULL, 
                             similarity_measures=c("jaccardSimilarity", "cosineSimilarity"),
                             correct_vec=NULL,
                             model_type=NULL, 
                             k=3,
                             up_down_weight="down"){
  
  # check basics of function by user
  if(is.null(dataframe) | is.null(formula)){
    error("You need to provide a dataframe and formula for the regression comparison.")
  }
  if(is.null(model_type)){
    error("You need to provide a family/model type (currently support OLS and logit).")
  }
  
  # calculate average weighted similarity measure
  if(is.null(similarity_measures)){
    dataframe <- averageSimilarity(dataframe, similarity_measures=c("jaccardSimilarity", "cosineSimilarity"), k, up_down_weight)
  }
  dataframe <- averageSimilarity(dataframe, similarity_measures, k, up_down_weight)
  if(model_type=="logit"){
    attention_glm <- glm(paste("avgSimilarity~", attention_formula), data=dataframe)
    correct_glm <- glm(paste(correct_vec, "~", attention_formula), data=dataframe)
  }
  # assign models to global environment
  # base models w/ all respondents
  assign(paste("attentionModel", sub('\\. *', '', attention_formula)[2], sep="_"),
         attention_glm, envir = .GlobalEnv)
  # removing inattentive participants based on correctness
  assign(paste("correctnessModel", sub('\\. *', '', attention_formula)[2], sep="_"),
         correct_glm, envir = .GlobalEnv)
  
  # print output of regressions 
  print(texreg(list(attention_glm, correct_glm),
                 custom.model.names = c("Attention", "Correct Response"),
                 digits=3, stars = c(0.001, 0.01, 0.05)))
  
  
}
