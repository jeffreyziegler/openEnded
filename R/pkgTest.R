#' @title Check to see if a package is installed
#' @description
#' This function checks if a dependent library is installed, and if not installs it, and then loads it.
#'
#'
#' @author Jeffrey Ziegler (<jeffrey.ziegler[at]emory.edu>)
#' @examples
#' lapply(c("stringdist", "ggplot2", "tidyr"), pkgTest)
#' 
#' @rdname pkgTest
#' @seealso \code{\link{loadDependencies}}
#' @export

pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = T)
  suppressMessages(sapply(pkg, require, character.only = T))
}
