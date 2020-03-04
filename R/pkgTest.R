#' @title Check to see if a package is installed
#' @description
#' This function checks if a dependent library is installed, and if not installs it, and then loads it.
#'
#'
#' @author Jeffrey Ziegler
#' @examples
#' 
#' \dontrun{
#' pkgTest(c("stringdist", "ggplot"))
#' }
#' @rdname pkgTest
#' @seealso \code{\link{pkgTest}}
#' @export

pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
