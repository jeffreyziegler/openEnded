#' @title plotSimilarity
#' 
#' @description 
#' This function plots the raw distribution for a given similarity measure.
#'
#' @param dataframe Dataframe that contains the similarity measures you want to plot.
#' @param measure Vector in dataframe that contains the similarity measures you want to plot.

#'
#' @author Jeffrey Ziegler
#' @examples
#' 
#' plotSimilarity(dataframe=replication_complete.cases, measure="jaccardDist")
#' 
#' @rdname plot_similarity
#' @seealso \code{\link{similarity_measures}}
#' @export

plotSimilarity <- function(dataframe, measure){
  ggplot(dataframe, aes(x = measure)) +  
    geom_histogram(binwidth=.025, colour="black", fill="white") + lims(x=c(0,1), y=c(0,150))+
    labs(x=paste(measure, "Similarity", sep=" "), y="Number of Respondents")+ theme_classic() +
    geom_vline(aes(xintercept=mean(jaccardDist, na.rm=T)),   # Ignore NA values for mean
               color="black", linetype="dashed", size=1.5) +
    theme(legend.position="none", axis.title = element_text(size=30), axis.text = element_text(size=25))  
}