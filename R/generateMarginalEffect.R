#' @title Generate Marginal Effects
#' 
#' @description 
#' Internal function that helps calculate the overall treatment effects differing how inattentive participants are down-weighted.
#' 
#' @param unique_covars Model matrix of unique characteristics used to generate treatment effects.
#' @param simulated_betas −XBetas that have been simulated from mvrnorm distribution
#' @param diff_labs 
#' @param model_type Statistical model to estimate. Currently support OLS and logistic ("ls", "logit").
#' @param plotDifferences Do you want to see the marginal effects by model, or the differences between the models with regard to their marginal effects? Default=FALSE.

#' 
#' @return Dataframe of marginal effects with corresponding 95% confidence intervals.
#'
#' @author Jeffrey Ziegler (<jeffrey.ziegler[at]emory.edu>)
#' @examples
#' generateMarginalEffect(generateMarginalEffect(unique_covars = unique_dummies, 
#' simulated_betas=sim_betas, diff_labs=fd_labs[,1]))
#' 
#' @seealso  \code{\link{regressionComparison}}
#' 
#' @rdname generateMarginalEffect
#' @export

generateMarginalEffect <- function(unique_covars, simulated_betas, diff_labs, model_type, plotDifferences){
  # go over all the possible combos of treatments
  predicted_probs <- matrix(ncol=dim(simulated_betas)[2], nrow=dim(simulated_betas)[1])
  for(covar_level in 1:dim(unique_covars)[1]){
    predicted_probs[, covar_level] <- simulated_betas%*%unique_covars[covar_level, ]
  }
  # now we have each "scenario" as a column w/ all
  # n simulations (default=10,000), so the first should be 
  # intercept, which is control and reference category
  # for a simple interaction b/w a categorical variable
  # and treatment (i.e. control independent)
  colnames(predicted_probs) <- colnames(simulated_betas)
  
  # next, go over each scenario and compare it to the others
  # to get 1st diffs. Some are plausible, others are not
  # Democrat from alienate to appease (predicted_probs[1,] - predicted_probs[4,])
  # or Independent control to Democrat control, which is not plausible
  
  # should be 36 combos for example
  # 9! / 2! * (9 - 2)!
  # (9*8) / factorial(2)
  level_combos <- as.data.frame(t(combn(diff_labs, 2, simplify=T)))
  combo_labs <- level_combos %>% unite("combo", V1:V2, na.rm = T, remove = F, sep="_")
  combo_labs <- combo_labs[,"combo"]
  #browser()
  if(model_type=="ols"){
    sim_diffs <- data.frame(t(
      matrix(
        unlist(
          comboGeneral(ncol(predicted_probs), 2, repetition = F, FUN = function(y){
            predicted_probs[,y[2]] - predicted_probs[,y[1]]
          }
          )
        ), 
        nrow=choose(ncol(predicted_probs), 2),
        byrow=T)
    ))
  }
  if(model_type=="logit"){
    sim_diffs <- data.frame(t(
      matrix(
        unlist(
          comboGeneral(ncol(predicted_probs), 2, repetition = F, FUN = function(y){
            (1/(1+exp(-predicted_probs[,y[2]]))) - (1/(1+exp(-predicted_probs[,y[1]])))
          }
          )
        ), 
        nrow=choose(ncol(predicted_probs), 2),
        byrow=T)
    ))
  }
  
  names(sim_diffs) <- combo_labs
  # create df fill with point estimates, lower and upper bounds
  CI_data <- data.frame(covar_cats = rep(NA, length(combo_labs)),
                        first_diffs = rep(NA, length(combo_labs)),
                        lower_CI = rep(NA, length(combo_labs)),
                        upper_CI = rep(NA, length(combo_labs)))
  for(i in 1:length(combo_labs)){
    CI_data[i, "first_diffs"] <- mean(sim_diffs[,i])
    CI_data[i, "covar_cats"] <- names(sim_diffs)[i]
    CI_interval <- quantile(sim_diffs[,i], 
                            probs=c((1-0.95)/2, (1-(1-0.95)/2)))
    CI_data[i, "lower_CI"] <- CI_interval[1]
    CI_data[i, "upper_CI"] <- CI_interval[2]
    
  }
  if(plotDifferences==T){
    return(sim_diffs)
  }
  if(plotDifferences!=T){
    return(CI_data)
  }
}
 