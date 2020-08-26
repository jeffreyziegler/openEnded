#' @title Simulate the ATE for Compliers and Non-Compliers
#' @description 
#' Plot the marginal effects for respondents that likely received and did not receive the treatment.
#' 
#' @param dataframe Dataframe from which we will estimate our regression model.
#' @param similarity_measures Vector(s) from dataframe that contains the similarity measures to be used as weights. Possible values for measure_type = c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex"). Default is "jaccard".
#' @param formula Symbolic representation of the model to be estimated. This is written in "typical" R language (i.e. y ~ x1 + x2), such that y is the outcome variable and x1 and x2 are the predictors.
#' @param k The penalty that you want to set for down-weighting inattentive respondents. Lower levels of k down-weight low attention participants more severely. 
#' @param model_type Statistical model to estimate. Currently support OLS and logistic ("ls", "logit").
#' @param bounds Minimum and maximum of uniform distribution we should draw cutoff values between.
#' @param n Number of simulation rounds/iterations.
#' 
#' @return Plot of the marginal effects for "compliers" and "non-compliers".
#'
#' @author Jeffrey Ziegler (<jeffrey.ziegler[at]emory.edu>)
#' @examples
#' plotComplierATE(dataframe=replication_complete.cases, similarity_measures=c("jaccardDist", "cosineDist"), bounds=c(0.05, 0.2), n=2, seed=12345, k=3, formula=list(trustLM, responsiveLM), model_type="ls")
#' @rdname plotComplierATE
#' @export

complierATE <- function(dataframe=NULL, 
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
  
  bootstrappedData <- data.frame()
  set.seed(user_seed)
    for(i in 1:n){
      bootstrappedData <- rbind(bootstrappedData, ATEcutoff(dataframe, 
                                                            formula,
                                                            similarity_measures, 
                                                            model_type,
                                                            k, 
                                                            bounds,
                                                            user_seed,
                                                            plot_treatment,
                                                            plot_interact_x, 
                                                            stable_x))
    }
  
    p1 <- ggplot(bootstrappedData, aes(x=first_diffs, y=as.factor(treat_from_to), colour=subset, fill=subset)) +
    theme_pubr() +
    geom_vline(aes(xintercept=0), linetype="dashed", size=.5, colour="black") +
    geom_density_ridges(quantile_lines = F,  alpha=.75, scale=.9) +
    facet_wrap(~interact_x, ncol=3, scales="fixed") + 
    scale_colour_grey(start = 0.2, end = 0.7)+
    scale_fill_grey(start = 0.2, end = 0.7)+
    lims(x=c(min(bootstrappedData$first_diffs)-0.1, max(bootstrappedData$first_diffs)+0.1))+
    theme(axis.title=element_text(size=20), axis.text = element_text(size=18), legend.text=element_text(size=18),
          strip.text = element_text(size=20), strip.background = element_rect(fill = NA, color = "black"),
          legend.position="bottom", legend.title = element_text(size=20),
          title = element_text(size=25),  legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"), axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_blank(), 
          panel.background = element_rect(fill = NA, color = "black"),
          panel.grid = element_blank()
    ) + 
    geom_vline(xintercept = c(1.5,2.5)) +
    labs(x='\nMarginal Effect of Treatment\n', y='\nTreatment Condition\n', colour="Sample:", fill="Sample:"
    )
  if(display_plot==T){
    print(p1)
  }
  if(!is.null(plot_path)){
    pdf(plot_path, width=11, height=7)
    print(p1)
    dev.off()
  }
}
