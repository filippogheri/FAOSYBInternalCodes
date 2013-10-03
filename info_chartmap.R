##' Get few info about the chart
##'
##' A function to get some basic info about the data chart
##'
##' @param dissemination The dissemination file.
##' @param data The dataset.
##' @param objectName The name of the object.
##' @export
##' @examples
##'
info_chartmap = function(dissemination = diss.df, data = final.df, 
                         objectName, variable = NULL) {
  if (!is.null(variable)) {
    variables = variable
  } else {
    object = subset(dissemination, subset = OBJECT_NAME == objectName)
    variables = object[, paste("DATA_KEY", 1:10, sep = "")]
    variables = variables[!is.na(variables)]
  }

  for (i in variables) {
    if (i %in% colnames(data)) {
      printLab(paste("Variable ", i, " available:", sep = ""), span = TRUE)
      uniqueYear = 
        unique(na.omit(subset(data, 
                              select = c("FAOST_CODE", i, "Year")))[, "Year"])
      cat("available years: ", uniqueYear, "\n\n")
      tmp = na.omit(subset(data, 
                           select = c("FAO_TABLE_NAME", "Year", i, "Area"),
                           subset = Year == max(uniqueYear)))      
      missingAgg = 
        arrange(unique(subset(data, 
                     subset = Year == max(uniqueYear) &
                       !FAO_TABLE_NAME %in% unique(tmp$FAO_TABLE_NAME) & 
                       !Area %in% c("Territory", "LACterritory", "REUterritory",
                                    "RAPterritory", "RNEterritory", "LACterritory",
                                    "oldTerritory"),
                     select = c("FAO_TABLE_NAME", "Area"))), Area)
      if (length(missingAgg) != 0) {
        cat("Missing aggregates for the last year available:\n")
        print(missingAgg)
      }
    } else {
      printLab(paste("Variable ", i, " NOT available:", sep = ""), span = TRUE)
    }
  }
}