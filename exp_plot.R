##' Export plot in latex code
##'
##' A function to translate R plots in Latex code
##'
##' @param output Where the plot has to be stored
##' @param name The name of the plot
##' @param width The width of the plot
##' @param height The height of the plot
##' @export
##' @return A plot in Latex code.

exp_plot = function(output, name, width = 3.3, height = 3.3) {
  require(tikzDevice)
  outChart = paste(output, name, ".tex", sep = "")
  tikz(outChart, width = width, height = height, sanitize = TRUE)
  print(eval(parse(text = name)))
  invisible(dev.off())
  cat("Chart", name, "successfully exported\n")
}
