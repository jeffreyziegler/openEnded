#' @title Plot Similarity Measures
#' 
#' @description 
#' This function plots the raw distribution for a given similarity measure.
#'
#' @param dataframe Dataframe that contains the similarity measures you want to plot.
#' @param measure Name of vector in dataframe that contains the similarity measures you want to plot.
#' @param xlab Character/string of name user wants label of x-axis to be.
#'
#' @author Jeffrey Ziegler
#' @examples
#' 
#' 
#' @rdname plotSimilarity
#' @seealso \code{\link{similarityMeasures}} \code{\link{plotSimilarityCorr}}
#' @export

plotSimilarity <- function(dataframe, measure, plot_path=NULL){
  # seting lims on x=[0,1] gives warnings
  # so we'll depress them for now and then turn them back on
  defaultW <- getOption("warn") 
  options(warn = -1) 
  p1 <- ggplot(dataframe, aes(x = dataframe[, measure])) +  
    geom_histogram(binwidth=.025, colour="black", fill="white") + 
    lims(x=c(-0.05, 1))+
    labs(x=paste( "\n", str_to_title(gsub('([[:upper:]])', ' \\1', measure)), sep=""), y="Number of Respondents")+ theme_classic() +
    geom_vline(aes(xintercept=mean(dataframe[, measure], na.rm=T)),
               color="black", linetype="dashed", size=1.5) +
    theme(legend.position="none", axis.title = element_text(size=30), axis.text = element_text(size=25))  

  # check to see if user wants to save plot
  if(!is.null(plot_path)){
    pdf(file=plot_path)
    print(p1)
    dev.off()
  }
  return(p1)
  # turn warnings back on
  options(warn = defaultW)
}
