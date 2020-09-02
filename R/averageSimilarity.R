#' @title Calculate Average Similarity
#' @description 
#' Calculate the average weighted similarity for all participants.
#' 
#' @param dataframe Dataframe from which you will select the similarity measures to be used to calculate the weights.
#' @param similarity_measures Vector(s) from dataframe that contains the similarity measures to be used as weights. 
#' @param k The penalty that you want to set for down-weighting inattentive respondents. Lower levels of k down-weight low attention participants more severely. 
#' @param up_down_weight Do you want to up-weight or down-weight?
#' 
#' @return Original dataframe with an additional vector that is the averaged weight of the similarity measures provided.
#' 
#' @author Jeffrey Ziegler (<jeffrey.ziegler[at]emory.edu>)
#' @examples
#' averageSimilarity(dataframe=replication_complete.cases, similarity_measures=c("jaccardDist", "cosineDist"),  k=3)
#' 
#' @seealso \code{\link{regressionComparison}}
#' 
#' @rdname averageSimilarity
#' @export

averageSimilarity <- function(dataframe, similarity_measures, k, up_down_weight="down"){
    
    # create empty vector to fill with each component of the "average" similarity
    additive_similarity <- NULL
    # loop over all similarity measures user specified in similarity_measures
    for(measure in similarity_measures){
        # take 1-each measure times their proportion to the average
        additive_similarity <- cbind(additive_similarity, (1-dataframe[, measure])*(1/length(similarity_measures)))
    }
    if(up_down_weight=="down"){
        # sum all of the similarities up and take 1 - mean(1-similarity)^k
        # and add column of average similarity to original dataframe
        dataframe[, "avgSimilarity"] <- 1 - (rowSums(additive_similarity)^k)
    }
    if(up_down_weight=="up"){
        # sum all of the similarities up and take 1/mean(1-similarity)
        # and add column of average similarity to original dataframe
        dataframe[, "avgSimilarity"] <- 1/(rowSums(additive_similarity))
    }
    
    return(dataframe)
}
