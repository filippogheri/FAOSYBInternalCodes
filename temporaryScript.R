########################################################################
## This script contains function which are used to generate the
## Statistical year book but should not be published.
########################################################################


library(zoo)
na.approx.syb = function (object, x = index(object), xout = x, ...,
    na.rm = TRUE, maxgap = Inf, along){
        if (!missing(along)) {
            warning("along to be deprecated - use x instead")
            if (missing(x))
                x <- along
        }
        na.approx.vec <- function(x, y, xout = x, ...) {
            if(length(na.omit(y)) < 2){
                y
            } else {
                na <- is.na(y)
                yf <- approx(x[!na], y[!na], xout, ...)$y
                if (maxgap < length(y)) {
                    ygap <- .fill_short_gaps(y, seq_along(y), maxgap = maxgap)
                    ix <- approx(x, seq_along(y), xout, ...)$y
                    yx <- ifelse(is.na(ygap[floor(ix)] + ygap[ceiling(ix)]),
                                 NA, yf)
                    yx
                }
                else {
                    yf
                }
            }
        }
        if (!identical(length(x), length(index(object)))) {
            stop("x and index must have the same length")
        }
        x. <- as.numeric(x)
        if (missing(xout) || is.null(xout))
            xout <- x.
        xout. <- as.numeric(xout)
        object. <- coredata(object)
        result <- if (length(dim(object.)) < 2) {
            na.approx.vec(x., coredata(object.), xout = xout., ...)
        }
        else {
            apply(coredata(object.), 2, na.approx.vec, x = x., xout = xout.,
                  ...)
        }
        if (na.rm) {
            result <- na.trim(result, is.na = "all")
        }
    result
}

sybImpute = function(var, country, year, data, maxgap = 3){
    sortData.df = arrange(data, get(country), get(year))
    uCountry = unique(sortData.df[, country])
    n.var = length(var)
    pb = txtProgressBar(min = 0, max = n.var, style = 3)
    for(j in 1:n.var){
        yearRange = range(sortData.df[!is.na(sortData.df[, var[j]]), year])
        years = yearRange[1]:yearRange[2]
        for(i in 1:length(uCountry)){
            sortData.df[sortData.df[, country] == uCountry[i] &
                        sortData.df[, year] %in% years, var[j]] =
                na.locf(na.approx.syb(sortData.df[sortData.df[, country] == uCountry[i] &
                                                  sortData.df[, year] %in% years, var[j]],
                                      na.rm = FALSE),
                        na.rm = FALSE, maxgap = maxgap)
        }
        setTxtProgressBar(pb, j)
    }
    sortData.df
}
