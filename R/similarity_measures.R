similarity_measures <-
function(dataframe, prompt, response, measure_type="jaccard", ngrams){
  # possible values for measure_type = c("osa", "lv", "dl", "hamming", "lcs",
  # "qgram", "cosine", "jaccard", "jw", "soundex")
  1-stringdist(enc2utf8(dataframe[, prompt]), enc2utf8(dataframe[, response]), useBytes=TRUE,  method=measure_type, q=ngrams)
}
