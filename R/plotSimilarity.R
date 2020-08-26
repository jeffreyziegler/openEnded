#' @title plotSimilarity
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
#' brazil_rows <- which(replication_complete.cases$Country=="Brazil")
#' plotSimilarity(dataframe=replication_complete.cases[brazil_rows,],
#'             measure="jaccardDist", xlab="Jaccard Similarity")
#' 
#' @rdname plotSimilarity
#' @seealso \code{\link{similarityMeasures}} \code{\link{plotSimilarityCorr}}
#' @export

plotSimilarity <- function(dataframe, measure, plot_path=NULL){
  p1 <- ggplot(dataframe, aes(x = dataframe[, measure])) +  
    geom_histogram(binwidth=.025, colour="black", fill="white") + 
    labs(x=paste( "\n", str_to_title(gsub('([[:upper:]])', ' \\1', measure)), sep=""), y="Number of Respondents")+ theme_classic() +
    geom_vline(aes(xintercept=mean(dataframe[, measure], na.rm=T)),
               color="black", linetype="dashed", size=1.5) +
    theme(legend.position="none", axis.title = element_text(size=30), axis.text = element_text(size=25))  
  return(p1)
  
  # check to see if user wants to save plot
  if(!is.null(plot_path)){
    pdf(file=plot_path)
    print(p1)
    dev.off()
  }
}
