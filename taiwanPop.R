##' This function compute population for Taiwan
##'
##' WB doesn't report explicitly the population for Taiwan. However, 
##' this is given by the difference from the agg "World" reported by the
##' WB and the sum of all the countries.
##'
##' @param WB.lst The list deriving from the getWDItoSYB function.
##' @export

taiwanPop = function(WB.lst) {
  sum2 = function(x) {sum(x, na.rm = any(!is.na(x)))}
  WBcountries = WB.lst$entity
  WBaggs = WB.lst$aggregates
  twn.df = data.frame(matrix(NA, nrow = length(unique(WBcountries$Year)), 
                             ncol = ncol(WBcountries)))
  colnames(twn.df) = colnames(WBcountries)
  twn.df$ISO2_WB_CODE = "TW"
  twn.df$Country = "Taiwan"
  twn.df$Year = unique(WBcountries$Year)
  for (i in 1:nrow(twn.df)) {
    countrySum = sum2(subset(WBcountries, 
                             subset = Year == twn.df[i, "Year"], 
                             select = "SP.POP.TOTL"))
    world = subset(WBaggs, 
                   subset = Year == twn.df[i, "Year"] & Country == "World", 
                   select = "SP.POP.TOTL")
    twn.df[i, "SP.POP.TOTL"] = world-countrySum
  }
  
  WB = rbind(WBcountries, twn.df)
  WB.lst$entity = WB
  WB.lst
}