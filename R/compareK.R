#' @title Determine "best" level of k
#' 
#' @description Calculate the correlation between the "correct" answer as determined by a human and participants' average similarity score 
#' 
#' @details 
#' Determine what value of k is best suited to reduce the impact of inattentive participants on the overall results, while still maintaining that our measure of attention is correlated with some indicator of correctness (even if it is subjective). 
#'
#' @param dataframe Dataframe from which you will select the similarity measures from.
#' @param correct_vec Character indicating vector in dataframe that indicates whether a respondent answered "correctly" as determined by a human coder. Default is NULL, so if a user doesn't include anything function will automatically set a threshold of "correctness" based on a respondent's average similarity (i.e. those respondents that score below say 0.1 will not be included in the "list-wise deletion" sample.
#' @param similarity_measures Vector(s) from dataframe that contains the similarity measures to be used as weights. 
#' @param k_range The range of penalties that you want plotted. Remember, lower levels of k down-weight low attention participants more severely. 
#' @param plot_path If user wants to save figure, please provide a character vector for the file path in which the plot should be download. User must decide extension (pdf, jpg, png) in file path. 

#' @return 
#' 
#' @author Jeffrey Ziegler (<jeffrey.ziegler[at]emory.edu>)
#' 
#' @examples
#' compareK(dataframe=kaneData, 
#'   similarity_measures =c("jaccardSimilarity", "cosineSimilarity"), 
#'   correct_vec = "correct", 
#'   k_range=1:10)
#' 
#' @seealso \code{\link{similarityMeasures}}
#' 
#' @rdname compareK
#' @export

compareK <- function(dataframe, similarity_measures, correct_vec, k_range=1:10, plot_path=NULL){
  # calculate average weighted similarity measure
  if(is.null(similarity_measures)){
    similarity_measures <- c("jaccardSimilarity", "cosineSimilarity")
  }

  # store correlation b/w average similarity 
  # and the correct "correct" varying/iterating over k
  cor_store <- NULL
  # we'll just do k=[1:10]
  for(k in k_range){
    cor_store <- c(cor_store, 
                   cor(dataframe[ ,correct_vec],
                       # automatically set to down weight since we're comparing k
                       averageSimilarity(dataframe, similarity_measures, k, up_down_weight="down")[,ncol(averageSimilarity(dataframe, similarity_measures, k, up_down_weight="down"))])
                   )
  }

  cor_data <- as.data.frame(cbind(cor_store, k_range))
 
  p1 <- ggplot(data=cor_data, aes(y=cor_store, x=k_range)) + geom_line() + 
    labs(x="Value of k", y="Correlation Between 'Correct' Answer\n& Average Similarity Score")+
    theme_pubr() +     scale_x_continuous(breaks=seq(0, 10, by=2)) +
    theme(axis.title=element_text(size=20), axis.text = element_text(size=18), legend.text=element_text(size=18),
          strip.text = element_text(size=20), strip.background = element_rect(fill = NA, color = "black"),
          legend.position="bottom", legend.title = element_text(size=20),
          title = element_text(size=25),  legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"), axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_blank(),
          panel.background = element_rect(fill = NA, color = "black"),
          panel.grid = element_blank()
    )
  # check to see if user wants to save plot
  if(!is.null(plot_path)){
    pdf(file=plot_path)
    print(p1)
    dev.off()
  }
  print(p1)
  
}
