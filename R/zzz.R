.First <- function(){
  cat("Hello!") # startup message
  
  #require(data.table)   
  # or whatever packages you want to load 
  
  # or if you want to run a function in a file
  if(file.exists("~/loadDependencies.R")){
    source("~/loadDependencies.R")
    myfunc()
  }
  
}