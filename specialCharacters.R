##' Avoid encoding problems for some special UTF-8 characters
##'
##' The function substitutes misread UTF-8 characters with the
##' proper simbols. This issue is not solved by the option 
##' "sanitize = TRUE" within the function tikz. Moreover, we
##' cannot use the function sanitizeToLatex because of tikz.
##'
##' @param output Where the plot is.
##' @param name Name of the plot.  
##' @export

specialCharacters = function(output, name) {
  fileOut = paste(output, name, ".tex", sep = "")
  
  tmp = read.table(fileOut, sep="\n", header = FALSE, 
                   col.names = "LatexCode", stringsAsFactors = FALSE)
  
  tmp$LatexCode = gsub("ô", "\\^{o}", tmp$LatexCode, fixed = TRUE)
  tmp$LatexCode = gsub("è", "\\`{e}", tmp$LatexCode, fixed = TRUE)
  tmp$LatexCode = gsub("é", "\\'{e}", tmp$LatexCode, fixed = TRUE)
  tmp$LatexCode = gsub("ç", "\\c{c}", tmp$LatexCode, fixed = TRUE)
  tmp$LatexCode = gsub("Å", "\\r{A}", tmp$LatexCode, fixed = TRUE)
  
  if(file.exists(fileOut)) file.remove(fileOut)
  file.create(fileOut)
  
  for (i in 1:nrow(tmp)) {
    cat(tmp[i,], "\n", file = fileOut, append = TRUE, sep = "")
  }
}
