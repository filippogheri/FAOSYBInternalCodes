##' This function processes Sudan for maps
##'
##' This function simply says that if we have data for Sudan (former) 
##' then impute these data to Sudan and South Sudan (that will be 
##' coloured in the same way).
##'
##' The function only work for FAOST_CODE, if the country coding
##' system is not in FAOST_CODE then use the translateCountryCode
##' function to translate it.
##'
##' @param var The variables that require to be sanitized.
##' @param data The data frame which contains the data
##' @param year The column which correspond to the year.
##' @export

SUDANinMAP = function(var, data, year = "Year"){
  for(i in 1:length(var)){
    if(length(unique(data[data$FAOST_CODE %in% c(206) &
                            !is.na(data[, var[i]]), "FAOST_CODE"])) == 1) {
      for(j in sort(unique(data[, year]))){
        if (NROW(data[data$FAOST_CODE %in% c(206) &
                        !is.na(data[, var[i]]) & 
                        data$year == j, "FAOST_CODE"]) == 1) {
          ## Remove Sudan (former)
          data[data$FAOST_CODE %in% c(276) & data[, year] == j, var[i]] = 
            data[data$FAOST_CODE %in% c(206) & data[, year] == j, var[i]]
          data[data$FAOST_CODE %in% c(277) & data[, year] == j, var[i]] = 
            data[data$FAOST_CODE %in% c(206) & data[, year] == j, var[i]]
        }
      }
    }
  }
  data
}

# SUDANinMap = function(var, data, year = "Year"){
#   for(i in 1:length(var)){
#     for(j in sort(unique(data[, year]))){
#       sf = FALSE; s = FALSE; ss = FALSE
#       if(NROW(data[data$FAOST_CODE %in% c(206) &
#                      data[, year] == j, var[i]]) == 1){
#         if(!is.na(data[data$FAOST_CODE %in% c(206) &
#                          data[, year] == j, var[i]])) {
#           ## Sudan (former) exists and is not NA
#           sf = TRUE
#         }
#       }
#       if (isTRUE(sf)) {
#         data[data$FAOST_CODE %in% c(276) & data[, year] == j, var[i]] = 
#           data[data$FAOST_CODE %in% c(206) & data[, year] == j, var[i]]
#         data[data$FAOST_CODE %in% c(277) & data[, year] == j, var[i]] = 
#           data[data$FAOST_CODE %in% c(206) & data[, year] == j, var[i]]
#         data[data$FAOST_CODE %in% c(206) & data[, year] == j, var[i]] = NA
#       } 
#     }
#   }
#   data
# }
