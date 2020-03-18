.onLoad <- function(libname, pkgname) {
  print("Hello, welcome to openEnded!")
}

.onAttach <- function(libname, pkgname) {
  loadDependencies()
  invisible()
}