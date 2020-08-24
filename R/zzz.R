.onLoad <- function(libname, pkgname) {
  print("Hello, welcome to openEnded! Please be patient while I load my dependencies :)")
}

.onAttach <- function(libname, pkgname) {
  # load and install all dependency libraries
  lapply(c("stringdist",
           "stringr",
           "ggplot2",
           "tidyr",
           "data.table",
           "reshape2",
           "Zelig",
           "ggcorrplot",
           "texreg",
           "plyr",
           "MASS",
           "ggridges",
           "RcppAlgos",
           "tidyverse",
           "ggpubr"), pkgTest)
  invisible()
}