##' A function to read in the meta data input file
##'
##' The function reads in the file containing information about the
##' indicator used and the download path. A check is also performed to
##' determine whether the file is filled in correctly.
##'
##' @param file The name of the file
##' @param ... Additional arguments, see read.csv.
##' @return The function reads the input file and returns a list
##' containing 4 data frame.
##' \itemize{
##'   \item The FULL file.
##'   \item The WDI subset used for downloading World Bank data.
##'   \item The FAOSTAT subset used for downloading FAOSTAT data.
##'   \item The UNIT subset which is used to scale the data to base unit.
##' }
##' @export
read.FAOSYBmeta = function(file, ...){
  meta = read.csv(file = file, stringsAsFactors = FALSE,
    na.string = "", header = TRUE, ...)
  rawMeta = subset(meta, DATA_TYPE == "raw")

  WDI = subset(rawMeta, select = c("DATA_KEY", "WDINAME"),
    subset = SOURCE == "World Bank (WDI)")
  FAOSTAT = subset(rawMeta, select = c("DATA_KEY", "SQL_DOMAIN_CODE",
                           "SQL_ELEMENT_CODE", "SQL_ITEM_CODE"),
    subset = SOURCE == "FAO, Statistics Division (FAOSTAT)" & DATA_TYPE == "raw")
  OTHER = subset(rawMeta, !(SOURCE %in%
    c("World Bank (WDI)", "FAO, Statistics Division (FAOSTAT)")))
  UNIT_MULTI= subset(meta, select = c("DATA_KEY", "QUANTITY"))
  list(FULL = meta, WDI = WDI, FAOSTAT = FAOSTAT, OTHER = OTHER,
       UNIT_MULTI = UNIT_MULTI)
}
