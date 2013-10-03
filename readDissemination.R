##' Function to read the dissemination file
##'
##' A function to read in the dissemination file nad perform check
##'
##' @param file The name of the dissemination file
##' @param ... Additional arguments, see read.csv.
##' @export
read.FAOSYBdiss = function(file = "diss.csv", ...){
  diss = read.csv(file = file, stringsAsFactors = FALSE,
    na.string = "", header = TRUE, ...)
  diss
}
