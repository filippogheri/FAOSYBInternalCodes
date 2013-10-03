##' Validation
##'
##' A function for determining whether the data in the long format (for
##' plots) satistfy our definition.
##'
##' @param data The data frame to be tested
##' @return prints out whether the data set passes the tests

validFaoData = function(data){
  printLab("Testing data definition")

  cat("Test 1: Regions defined? ")
  if(any(c("FAOST_CODE", "AreaCode") %in% colnames(data))){
    ind = grep("FAOST_CODE|AreaCode", colnames(data))
    cat("PASS\n")
  } else {
    cat("FAIL\n")
  }
  cat("Test 2: Years well defined? ")
  if("Year" %in% colnames(data)){
    if(sum(is.na(data["Year"])) == 0){
      cat("PASS\n")
    } else
    cat("FAIL - Missing values in Year\n")
      } else {
        cat("FAIL - column Year does not exists\n")
      }
  cat("Test 3: Values unique? ")
  if("Value" %in% colnames(data)){
    tmp = with(data, aggregate(Value, list(Year, data[, ind], Indicator),
                                length))
    if(all(tmp$x == 1)){
      cat("PASS\n\n")
    } else {
      cat("FAIL - Values are not unique\n\n")
    }
  } else {
    cat("FAIL - Value column does not exists\n\n")
  }
}
