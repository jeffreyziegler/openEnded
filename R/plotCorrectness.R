#' @title plotCorrectness
#' 
#' @description 
#' 
#'
#' @param dataframe Dataframe that contains the similarity measures you want to plot.
#' @param measures Two characters representing the vectors in dataframe that contain the similarity measures you want to compare.
#' @param correct_vec Character indicating vector in dataframe that indicates whether a respondent answered "correctly" as determined by a human coder. Must have this for plot to be used, Default=NULL.
#'
#' @author Jeffrey Ziegler
#' @examples
#' 
#'
#' 
#' @rdname plotCorrectness
#' @seealso \code{\link{similarityMeasures}} \code{\link{plotSimilarity}}
#' @export

plotCorrectness <- function(dataframe, measures, correct_vec, plot_path=NULL){
  p1 <- ggplot(dataframe, aes(x=dataframe[, measures[1]], y=dataframe[, measures[2]], 
                              colour=as.factor(dataframe[, correct_vec]), 
                              shape=as.factor(dataframe[, correct_vec]))) +
    geom_point(size=1.25, alpha=.8) +
    scale_colour_manual(values = c("grey40", "black"))+
    scale_shape_manual(values = c(17, 1))+
    labs(x=paste("\n", str_to_title(gsub('([[:upper:]])', ' \\1', measures[1])), sep=""), 
         y=paste(str_to_title(gsub('([[:upper:]])', ' \\1', measures[2])), "\n", sep=""), 
         colour="Recalled Text:", 
         shape="Recalled Text:") + 
    lims(x=c(0,1), y=c(0,1)) +
    theme_pubr()  + theme(legend.position = "bottom", 
                          #legend.position = "bottom", 
                          legend.title=element_text(size=20), legend.text=element_text(size=18),
                          # panel.grid.major = element_line(size = 0.15, linetype = 'solid', colour = "grey80"),
                          legend.box.background = element_rect(colour = "black"),
                          axis.title = element_text(size=25), axis.text = element_text(size=20)) +
    guides(colour = guide_legend(override.aes = list(size=3)))
   
  print(p1)
  # check to see if user wants to save plot
  if(!is.null(plot_path)){
    pdf(file=plot_path)
    print(p1)
    dev.off()
  }
}

