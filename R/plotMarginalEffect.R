#' @title Plot Marginal Effect
#' @description 
#' Plot the marginal effects varying which respondents are down-weighted in a regression.
#' 
#' @param regression_models List that contains a vector or vectors of the regression models for which you wish to estimate the marginal effects. If you only want to plot one outcome at a time, you only need to include one vector (c(baseModel, listwiseModel, weightedModel)). If you wish to display more than one outcome at a time, this is possible with facets, but you need to place each outcome in a separate list in the argument (list(c(baseModel1, listwiseModel1,weightedModel1), c(baseModel2, listwiseModel2, weightedModel2))).
#'
#' @return Plot of the marginal effects for the full, listwise, and weighted samples given the formula the user specified.
#'
#' @author Jeffrey Ziegler (<jeffrey.ziegler[at]emory.edu>)
#' @examples
#' plotMarginalEffect(list(c(baseModel_trustChurch_postTreat, listwiseModel_trustChurch_postTreat, weightedModel_trustChurch_postTreat)))
#' plotMarginalEffect(regression_models=list(c(baseModel_trustChurch_postTreat, listwiseModel_trustChurch_postTreat, weightedModel_trustChurch_postTreat), 
#'                                           c(baseModel_responsiveness_postTreat, listwiseModel_responsiveness_postTreat, weightedModel_responsiveness_postTreat)))
#' 
#' @seealso  \code{\link{regressionComparison}} \code{\link{executeMarginalEffect}} \code{\link{generateMarginalEffect}} \code{\link{plotComplierATE}}
#' 
#' @rdname plotMarginalEffect
#' @export

plotMarginalEffect <- function(regression_models){
  firstDiffPlotData <- NULL
  sample_types <- c("base", "listwise", "weighted")
  for(i in 1:length(regression_models)){
    for(sample in 1:length(sample_types)){
      firstDiffPlotData <- rbind(firstDiffPlotData, executeMarginalEffect(subset=sample_types[sample], regression_model=regression_models[[i]][[sample]]))  
    }
  }
  
  firstDiffMeltedData <- reshape2::melt(firstDiffPlotData, id = c("subset", "outcome"))
  
  
  firstDiffMeltedData$subset <- revalue(firstDiffMeltedData$subset, c("base"="Full (OLS)",
                                                                      "weighted"="Full (WLS)",
                                                                      "listwise"="Participants weight >=0.1\n(List-wise Deletion OLS)"))
  firstDiffQuartilesPlotData <- cbind(ddply(firstDiffMeltedData, .(variable, outcome, subset), summarize, FDmean = mean(value)),
                                      FDlow90 = ddply(firstDiffMeltedData, .(variable, outcome, subset), summarize, FDlow = quantile(value, .05))[,4],
                                      FDhigh90 = ddply(firstDiffMeltedData, .(variable, outcome, subset), summarize, FDhigh = quantile(value, .95))[,4],
                                      FDlow95 = ddply(firstDiffMeltedData, .(variable, outcome, subset), summarize, FDlow = quantile(value, .025))[,4],
                                      FDhigh95 = ddply(firstDiffMeltedData, .(variable, outcome, subset), summarize, FDhigh = quantile(value, .975))[,4]
  )
  
  firstDiffQuartilesPlotData$marginalEffect <- gsub(".*\\(","", gsub("\\)","", firstDiffQuartilesPlotData$variable))
  firstDiffQuartilesPlotData$attendance <- trimws(gsub("\\(.*","", firstDiffQuartilesPlotData$variable), which="both")
  
  firstDiffQuartilesPlotData$subset <- factor(firstDiffQuartilesPlotData$subset, 
                                              levels = c("Full (OLS)", "Participants weight >=0.1\n(List-wise Deletion OLS)", "Full (WLS)"))
  
  firstDiffQuartilesPlotData$attendance <- factor(firstDiffQuartilesPlotData$attendance, 
                                                  levels = c("Never/yearly","Monthly", "Weekly"))
  firstDiffQuartilesPlotData$outcome <- str_to_title(gsub("_.*","", gsub('([[:upper:]])', ' \\1', firstDiffQuartilesPlotData$outcome)))
  ggplot(firstDiffQuartilesPlotData) +
    theme_pubr() +
    geom_pointrange(aes(x=attendance, y = FDmean, ymin = FDlow95, ymax = FDhigh95, colour=subset, shape=subset),
                    position = position_dodge(width =.75), size=1.5)+
    scale_shape_manual(values = c(17,18,19, 16))+
    geom_hline(aes(yintercept= 0), linetype="dashed", size=.5, colour="black") +
    facet_wrap(~outcome, ncol=3) + 
    scale_colour_grey(start = 0, end = 0.7)+
    theme(axis.title=element_text(size=20), axis.text = element_text(size=18), legend.text=element_text(size=18),
          strip.text = element_text(size=20), strip.background = element_rect(fill = NA, color = "black"),
          legend.position="bottom", legend.title = element_text(size=20),
          title = element_text(size=25),  legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"), axis.text.x = element_text(angle = 90, hjust = 1),
          panel.border = element_blank(),
          panel.background = element_rect(fill = NA, color = "black"),
          panel.grid = element_blank()
    ) +
    geom_vline(xintercept = c(1.5,2.5)) +
    labs(y='\nMarginal Effect of Treatment\n', x='\nTreatment\n', colour="Sample\n(Model):", shape="Sample\n(Model):")
}

