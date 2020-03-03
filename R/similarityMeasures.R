#' @title simlarityMeasures
#' @description
#' Clear global environment and detach all libraries
#' 
#' @details 
#' 
#' This function removes any hidden libraries or content that may be loaded in your global environment
#'
#' @param 
#'
#' # possible values for measure_type = c("osa", "lv", "dl", "hamming", "lcs",
#' "qgram", "cosine", "jaccard", "jw", "soundex")
#' @author Jeffrey Ziegler
#' @examples
#' 
#' \dontrun{
#' simlarityMeasures()
#' }
#' @rdname simlarityMeasures
#' @seealso 
#' @export

similarity_measures <- function(dataframe, prompt, response, measure_type="jaccard", ngrams){
  1-stringdist(enc2utf8(dataframe[, prompt]), enc2utf8(dataframe[, response]), useBytes=TRUE,  method=measure_type, q=ngrams)
}
