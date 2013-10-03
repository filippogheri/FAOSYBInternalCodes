##' This function avoids double counting of Sudan
##'
##' The function basically says: a) if Sudan (former) is not there, do 
##' nothing; b) if all the three Sudan are present, remove Sudan (former);
##' c) if Sudan (former) is there with just one of the other two, then
##' keep just Sudan (former).
##'
##' However, there is a problem when a variable has solution b) and 
##' the weighting variable has solution c), or vice versa. Nevertheless,
##' there is no a general rule we can adopt. Could be that the same
##' variable should be used with solution b for a certain variable but
##' with solution c for another one.
##'
##' The function only work for FAOST_CODE, if the country coding
##' system is not in FAOST_CODE then use the translateCountryCode
##' function to translate it.
##'
##' @param var The variables that require to be sanitized.
##' @param data The data frame which contains the data
##' @param year The column which correspond to the year.
##' @export

SUDAN = function(var, data, year = "Year"){
  for(i in 1:length(var)){
    if(length(unique(data[data$FAOST_CODE %in% c(206) &
                            !is.na(data[, var[i]]), "FAOST_CODE"])) == 1 &
         length(unique(data[data$FAOST_CODE %in% c(276,277) &
                              !is.na(data[, var[i]]), "FAOST_CODE"])) > 0) {
      cat("Multiple Sudan detected in '", var[i],
          "' sanitization is performed\n")
      for(j in sort(unique(data[, year]))){
        if (NROW(data[data$FAOST_CODE %in% c(206, 276, 277) &
                        !is.na(data[, var[i]]) & 
                        data[, year] == j, "FAOST_CODE"]) == 3) {
          ## Remove Sudan (former)
          data[data$FAOST_CODE %in% c(206) & data[, year] == j, var[i]] = NA
        } else {
          if (NROW(data[data$FAOST_CODE %in% c(276) &
                          !is.na(data[, var[i]]) & 
                          data[, year] == j, "FAOST_CODE"]) == 1) {
            ## Remove Sudan 276
            data[data$FAOST_CODE %in% c(276) & data[, year] == j, var[i]] = NA
          }
          if (NROW(data[data$FAOST_CODE %in% c(277) &
                          !is.na(data[, var[i]]) & 
                          data[, year] == j, "FAOST_CODE"]) == 1) {
            ## Remove Sudan 277
            data[data$FAOST_CODE %in% c(277) & data[, year] == j, var[i]] = NA
          }
        }
      }
    } 
  }
  data
}


# SUDAN = function(var, data, year = "Year", old = FALSE){
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
#       if(NROW(data[data$FAOST_CODE %in% c(276) &
#                      data[, year] == j, var[i]]) == 1){
#         if(!is.na(data[data$FAOST_CODE %in% c(276) &
#                          data[, year] == j, var[i]])) {
#           ## Sudan exists and is not NA
#           s = TRUE
#         }
#       }
#       if(NROW(data[data$FAOST_CODE %in% c(277) &
#                      data[, year] == j, var[i]]) == 1){
#         if(!is.na(data[data$FAOST_CODE %in% c(277) &
#                          data[, year] == j, var[i]])) {
#           ## South Sudan exists and is not NA
#           ss = TRUE
#         }
#       }
#       if (isTRUE(unique(c(sf,s,ss)))) {
#         ## Sudan (former), Sudan, & South Sudan in the dataset. Sudan (former)
#         ## set to NA.
#         data[data$FAOST_CODE %in% c(206) & data[, year] == j, var[i]] = NA
#       } else {
#         if (sf) {
#           ## at least one among Sudan and South Sudan is NA. Sudan & South
#           ## Sudan set to NA.
#           if (s) data[data$FAOST_CODE %in% c(276) & data[, year] == j, var[i]] = NA
#           if (ss) data[data$FAOST_CODE %in% c(277) & data[, year] == j, var[i]] = NA
#         } else {
#           ## we show whatever is in Sudan & South Sudan...but if old = TRUE
#           ## then we force to use Sudan (former)
#           if (old) {
#             if (s) {
#               data[data$FAOST_CODE %in% c(206) & data[, year] == j, var[i]] = 
#                 data[data$FAOST_CODE %in% c(276) & data[, year] == j, var[i]]
#               data[data$FAOST_CODE %in% c(276) & data[, year] == j, var[i]] = NA
#               if (ss) data[data$FAOST_CODE %in% c(277) & data[, year] == j, var[i]] = NA
#             }
#           }
#         }
#       }
#     }
#   }
#   data
# }