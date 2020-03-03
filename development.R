##################
### create package
##################

# create package skeleton
# i.e. new folder
setwd('~/Dropbox/Emory/projects/textAttentionCheck/package')

library(devtools); library(roxygen2); library(testthat)

# Put the *.R files into the correct directories and edit the DESCRIPTION file
# package.skeleton("openEnded")
my_package <- as.package("openEnded")
load_all(my_package); document(my_package)#; #check(my_package); 
build(my_package)
