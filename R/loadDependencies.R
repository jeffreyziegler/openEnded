#' @title loadDependencies
#' @description
#' Clear global environment and detach all libraries
#' 
#' @details 
#' 
#' This function removes any hidden libraries or content that may be loaded in your global environment
#'
#' @param packages A vector of package names that will be loaded as dependencies
#' @author Jeffrey Ziegler
#' @examples
#' 
#' \dontrun{
#' loadDependencies()
#' }
#' @rdname loadDependencies
#' @seealso \code{\link{pkgTest}}
#' @export

loadDependencies <- function() {
  # list basic packages and remove any unnecessary packages floating in environment
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  # remove objects
  rm(list=ls())
  # load and install all dependency libraries
  lapply(c("stringdist", "ggplot2", "tidyr"), pkgTest)
}