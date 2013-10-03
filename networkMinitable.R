##' Create minitable for network charts
##'
##' A function for creating mintable for network charts
##'
##' @param dissemination The dissemination file.
##' @param metadata The metadata file.
##' @param objectName The OBJECT_NAME in the dissemination file.
##' @param manual Optional. Specify the source.
##' @param output The output path.
##' @return The source for the specified object.
##' @export

networkMinitable = function(output, networkName, table, title) {
  
  fileOut = paste(output, networkName, ".tex", sep = "")
  if(file.exists(fileOut)) file.remove(fileOut)
  file.create(fileOut)
  
  latexMinitable = matrix(as.character(table))
  cat("\\scriptsize\n", file = fileOut, append = TRUE)
  cat("\\begin{center}\n", file = fileOut, append = TRUE)
  cat(title, file = fileOut, append = TRUE)
  cat("\\begin{tabular}{llr}\n", file = fileOut, append = TRUE)
  cat("\\hline\n", file = fileOut, append = TRUE)
  cat(paste(sanitizeToLatex(colnames(table[1])), 
            sanitizeToLatex(colnames(table[2])), 
            sanitizeToLatex(colnames(table[3])), sep = " & "), 
      file = fileOut, append = TRUE)
  cat("\\\\\n", file = fileOut, append = TRUE)
  cat("\\midrule\n", file = fileOut, append = TRUE)
  for (i in 1:nrow(table)) {
    cat(paste(sanitizeToLatex(table[i, 1]), 
              sanitizeToLatex(table[i, 2]), 
              sanitizeToLatex(table[i, 3]), sep = " & "), 
        file = fileOut, append = TRUE)
    cat("\\\\\n", file = fileOut, append = TRUE)
  }
  cat("\\hline\n", file = fileOut, append = TRUE)
  cat("\\end{tabular}\n", file = fileOut, append = TRUE)
  cat("\\end{center}\n", file = fileOut, append = TRUE)
}