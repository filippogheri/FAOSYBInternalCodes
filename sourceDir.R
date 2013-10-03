##' Read R scripts of a specific directory
##'
##' Function to read all the R scripts inside the specified
##' directory.
##'
##' @param path Directory path.
##' @param trace Display what have been sourced (default
##' equal to TRUE).

sourceDir = function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}