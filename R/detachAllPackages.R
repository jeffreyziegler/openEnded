#' @title detachAllPackages
#' @description
#' Clear global environment and detach all libraries
#' 
#' @details 
#' 
#' This function removes any hidden libraries or content that may be loaded in your global environment
#'
#'
#' @author Jeffrey Ziegler
#' @examples
#' 
#' \dontrun{
#' detachAllPackages()
#' }
#' @rdname detachAllPackages
#' @seealso \code{\link{pkgTest}}
#' @export

detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  # remove objects
  rm(list=ls())
}