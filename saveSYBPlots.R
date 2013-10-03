##' Save plots
##'
##' Save ggplot object in the workspace to pdf objects in the specified
##' directory
##'
##' @param dir The name of the directory to save the plots
##' @param name The name of the plot in the work space
##' @param ... Optional arguements to be passed to the pdf device
##' @examples
##' c <- ggplot(mtcars, aes(factor(cyl))) + geom_bar()
##' saveSYBPlots(name = "c", width = 5, height = 12)
saveSYBPlots = function(dir = NULL, name, ...){
  printLab("Saving plots")
  n = length(name)
  for(i in 1:n){
    if(name[i] %in% ls()){
      pdf(file = paste(dir, name[i], ".pdf", sep = ""), ...)
      print(get(name[i], envir = .GlobalEnv))
      graphics.off()
    }
  }
}
