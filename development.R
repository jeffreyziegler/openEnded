####################
### detach libraries
####################

detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}

##################
### load libraries
##################

pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}


##############################
### create similarity measures
##############################

similarity_measures <- function(dataframe, prompt, response, measure_type="jaccard", ngrams){
  # possible values for measure_type = c("osa", "lv", "dl", "hamming", "lcs",
  # "qgram", "cosine", "jaccard", "jw", "soundex")
  1-stringdist(enc2utf8(dataframe[, prompt]), enc2utf8(dataframe[, response]), useBytes=TRUE,  method=measure_type, q=ngrams)
}


##################
### create package
##################

# create package skeleton
# i.e. new folder
setwd('~/Dropbox/Emory/projects/textAttentionCheck/')

library(devtools)
library(roxygen2)
roxygenize("openEnded")
build('openEnded')
#######################
### execute source code
#######################

library(devtools)
library(roxygen2)

my.Rpackage <- as.package("openEnded")
load_all(my.Rpackage)
document(my.Rpackage)
