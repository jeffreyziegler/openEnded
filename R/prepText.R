#' @title Clean and Tokenize Text
#' @description
#' Remove punctuation, capitalization, tokenize, etc. text prior to calculating distance in a word embedding space.
#'
#'
#' @author Jeffrey Ziegler (<jeffrey.ziegler[at]emory.edu>)
#' @examples
#' 
#' 
#' @rdname prepText
#' @seealso \code{\link{}}
#' @export

prepText <- function(x) {
  x %>% 
    # make text lower case
    str_to_lower %>% 
    # remove all single quotes
    str_replace_all("'", "") %>%
    # remove non-alphanumeric symbols
    str_replace_all("[^[:alnum:]]", " ") %>% 
    # collapse multiple spaces
    str_replace_all("\\s+", " ")
}
