#' @title loadDependencies
#' @description
#' Clear global environment and detach all libraries
#' 
#' @details 
#' 
#' This function removes any hidden libraries or content that may be loaded in your global environment
#'
#' @param packages A vector of package names that will be loaded as dependencies
#' 
#' @author Jeffrey Ziegler (<jeffrey.ziegler[at]emory.edu>)
#' 
#' @examples
#' loadDependencies()
#' 
#' @rdname loadDependencies
#' @seealso \code{\link{pkgTest}}
#' @export

loadDependencies <- function() {
  # load and install all dependency libraries
  lapply(c("stringdist", "ggplot2", "tidyr", "Zelig", "ggcorrplot"), pkgTest)
}