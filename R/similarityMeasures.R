#' @title Individual Similarity Measures
#' @description 
#' Calculate individual measures of similarity between prompt and response in open-ended manipulation check.
#' 
#' @param dataframe Dataframe that contains the prompt and response of the open-ended manipulation check.
#' @param prompt Vector/column within `dataframe` that contains the prompt.
#' @param response Vector/column within `dataframe` that contains the open-ended response.
#' @param measure_type Possible values for measure_type = c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex"). Default is "jaccard".
#' @param ngrams The number of grams/segments words should be broken into. Default n=3.
#'
#' @return Vector of similarity measures (same length as input dataframe)
#'
#' @author Jeffrey Ziegler (<jeffrey.ziegler[at]emory.edu>)
#' 
#' @examples
#' simlarityMeasures(replication_complete.cases, prompt="textViewed", response="validityCheck", ngrams=3)
#' 
#' @seealso \code{\link{plotSimilarity}} \code{\link{plotSimilarityCorr}}
#' 
#' @rdname simlarityMeasures
#' @export

similarityMeasures <- function(dataframe, prompt, response, similarity_measures_to_calculate=NULL, ngrams=3){
  for(measure_to_calc in similarity_measures_to_calculate){
    dataframe[, paste(measure_to_calc, "Dist", sep="")] <-   1-stringdist(enc2utf8(dataframe[, prompt]), enc2utf8(dataframe[, response]), useBytes=TRUE,  method=measure_to_calc, q=ngrams)
  }
  return(dataframe)
}
