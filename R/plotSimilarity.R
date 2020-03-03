#' @title plotSimilarity
#' 
#' @description
#' Plot similarity measure
#' 
#' @details 
#' 
#' This function plots the raw distribution for a given similarity measure
#'
#'
#' @author Jeffrey Ziegler
#' @examples
#' 
#' \dontrun{
#' plotSimilarity(dataframe=kaneData, method="jaccard")
#' }
#' @rdname plot_similarity
#' @seealso \code{\link{similarity_measures}}
#' @export

plot_similarity <- function(dataframe, method="jaccard"){
  ggplot(dataframe, aes(x = jaccardDist)) +  
    geom_histogram(binwidth=.025, colour="black", fill="white") + lims(x=c(0,1), y=c(0,150))+
    labs(x=paste(method, "Similarity", sep=" "), y="Number of Respondents")+ theme_classic() +
    geom_vline(aes(xintercept=mean(jaccardDist, na.rm=T)),   # Ignore NA values for mean
               color="black", linetype="dashed", size=1.5) +
    theme(legend.position="none", axis.title = element_text(size=30), axis.text = element_text(size=25))  
}