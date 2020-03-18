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

plotSimilarity <- function(dataframe, measure, xlab){
  ggplot(dataframe, aes(x = dataframe[, measure])) +  
    geom_histogram(binwidth=.025, colour="black", fill="white") + 
    lims(x=c(0,1))+
    labs(x=xlab, y="Number of Respondents")+ theme_classic() +
    geom_vline(aes(xintercept=mean(jaccardDist, na.rm=T)),   # Ignore NA values for mean
               color="black", linetype="dashed", size=1.5) +
    theme(legend.position="none", axis.title = element_text(size=30), axis.text = element_text(size=25))  
}
