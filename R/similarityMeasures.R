#' @title Calculate Similarity Measures
#' @description 
#' Calculate individual or multiple measures of similarity between prompt and response in open-ended manipulation check.
#' 
#' @param dataframe Dataframe that contains the prompt and response of the open-ended manipulation check.
#' @param prompt Vector/column within `dataframe` that contains the prompt.
#' @param response Vector/column within `dataframe` that contains the open-ended response.
#' @param n_gram_measures_to_calculate Vector of n-gram similarity measures to calculate. Possible values for measure_type = c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex"). Default is "jaccard".
#' @param ngrams The number of grams/segments words should be broken into. Default n=3.
#'
#' @return Original dataframe plus columns with similarity measures (same length as input dataframe)
#'
#' @author Jeffrey Ziegler (<jeffrey.ziegler[at]emory.edu>)
#' 
#' @examples
#' replication_complete.cases <- simlarityMeasures(replication_complete.cases, prompt="textViewed", response="validityCheck", ngrams=3)
#' 
#' @seealso \code{\link{plotSimilarity}} \code{\link{plotSimilarityCorr}}
#' 
#' @rdname simlarityMeasures
#' @export

similarityMeasures <- function(dataframe, prompt, response, n_gram_measures_to_calculate=c("jaccard"), ngrams=3){
  for(measure_to_calc in n_gram_measures_to_calculate){
    dataframe[, paste(measure_to_calc, 
                      "Similarity", 
                      sep="")] <-   1-stringdist(enc2utf8(dataframe[, prompt]), 
                                                                                enc2utf8(dataframe[, response]), 
                                                                                useBytes=TRUE,  
                                                                                method=measure_to_calc, q=ngrams)
  }
  dataframe$cosineSimilarity <- ifelse(is.na(dataframe$cosineSimilarity), 0, dataframe$cosineSimilarity)
  return(dataframe)
}
