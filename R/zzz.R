.onLoad <- function(libname, pkgname) {
  print("Hello, welcome to openEnded! Please be patient while I load my dependencies :)")
}

.onAttach <- function(libname, pkgname) {
  # load and install all dependency libraries
  lapply(c("stringdist", "ggplot2", "tidyr", "Zelig", "ggcorrplot"), pkgTest)
  invisible()
}