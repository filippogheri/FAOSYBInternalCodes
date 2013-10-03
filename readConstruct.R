##' Function to read the construction file
##'
##' The function reads the construction file with forced default setting
##'
##' @param file The name of the construction rule file
##' @param ... Additional arguments, see read.csv
##' @export
read.FAOSYBcon = function(file = "construct.csv", ...){
  construct = read.csv(file = file, stringsAsFactors = FALSE,
    na.string = "", header = TRUE, ...)
  construct
}
