#' @title plotSimilarityCorr
#' 
#' @description 
#' This function plots the correlation between a given similarity measure.
#'
#' @param dataframe Dataframe that contains the similarity measures you want to plot.
#' @param measures Vector(s) in dataframe that contains the similarity measures you want to plot.
#' @param labels Character vector of labels for plot.
#' 
#' @return The figure will appear in your plot window.
#' 
#' @author Jeffrey Ziegler (<jeffrey.ziegler[at]emory.edu>)
#' @examples
#' 
#' plotSimilarityCorr(dataframe=replication_complete.cases, measures=c("jaccardDist", "cosineDist"))
#' 
#' @rdname plotSimilarityCorr
#' @seealso \code{\link{similarityMeasures}}  \code{\link{plotSimilarity}}
#' @export

plotSimilarityCorr <- function(dataframe, measures, labels){
  # subset to similarity measures in dataframe
  corrPlotData <- dataframe[, measures]
  # rename columns for plot
  names(corrPlotData) <- labels
  # execute correlation plot
  ggcorrplot(round(cor(corrPlotData, use="complete.obs"), 2), hc.order = T,
             type = "lower", lab = T, lab_size=5, outline.col = "black", 
             method="square",  legend.title = "Correlation",
             ggtheme = ggplot2::theme_classic, colors = c("grey90", "grey0", "grey90")) + 
    labs(y='', x="", color="Correlation") +
    theme(legend.position=c(.2, .8), legend.title=element_text(size=16), 
          legend.text=element_text(size=14), axis.text.y = element_text(size=18),
          axis.text.x = element_text(size=18, angle = 25, hjust = 1)) 
}
