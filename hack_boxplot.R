## Merge the plot data frame with the region.

hack_boxplot = function(x, y, year, scale = 1, data,
    countryName = "OFFICIAL_FAO_NAME", regionName = "UNSD_MACRO_REG",
    x_lab = NULL, y_lab = NULL, col = plot_colors(n = 5)$Sub, useWorld = TRUE,
    inPointRange){
    cty.df = data[data$Area == "Territory" & data$Year == year,
        unique(c(x, y, countryName, regionName))]
    if(useWorld){
        cty2.df = cty.df
        cty2.df[, regionName] = "World"
        cty2.df[, countryName] = ""
        cty.df = rbind(cty.df, cty2.df)
        reg.df = data[data$Area %in% c("Region", "World") & data$Year == year,
            unique(c(x, y, countryName, regionName))]
    } else {
        reg.df = data[data$Area == "Region" & data$Year == year,
            unique(c(x, y, countryName, regionName))]
    }

    reg.df[, y] = reg.df[, y] * scale

    if(missing(inPointRange)){
        ## cty.df = merge(cty.df, ddply(cty.df,
        ##     c(regionName), function(x) quantile(x[, y], c(0.25, 0.75), na.rm = TRUE)))

        ## colnames(cty.df)[colnames(cty.df) == "75%"] = "uQntl"
        ## colnames(cty.df)[colnames(cty.df) == "25%"] = "lQntl"
        ## cty.df$inQntl = with(cty.df, uQntl - lQntl)
        ## cty.df$ub = with(cty.df, uQntl + 1.5 * inQntl)
        ## cty.df$lb = with(cty.df, lQntl - 1.5 * inQntl)


        ## cty.df[, countryName] =
        ##     ifelse(cty.df[, y] >= cty.df[, "ub"] |
        ##            cty.df[, y] <= cty.df[, "lb"],
        ##            cty.df[, countryName], "")

        ## Replace by only labelling the maximu point
        cty.df = merge(cty.df, ddply(cty.df,
            c(regionName), function(x) max(x[, y], na.rm = TRUE)))
        colnames(cty.df)[colnames(cty.df) == "V1"] = "maximum"

        cty.df[, countryName] =
            ifelse(cty.df[, y] == cty.df[, "maximum"],
                   cty.df[, countryName], "")

    } else {
        cty.df[, countryName] =
            ifelse(cty.df[, y] >= inPointRange[2] |
                   cty.df[, y] <= inPointRange[1],
                   cty.df[, countryName], "")
    }
    reg.df[, y] = ifelse(reg.df[, y] >= 100, round(reg.df[, y]),
          round(reg.df[, y], 2))

    totMac = max(cty.df[, y], na.rm = TRUE) * 1.1
    ggplot(data = cty.df, aes_string(x = x, y = y)) +
           geom_boxplot(col = col, fill = col, outlier.size = 1.5,
                        outlier.colour = NULL) +
           geom_text(data = reg.df,
                     aes_string(x = x, y = eval(parse(text = totMac)), label = y),
                                size = 3, col = "tan4") +
                     labs(x = x_lab, y = y_lab) +
           geom_text(data = cty.df,
                     aes_string(label = countryName),
                     position = position_jitter(width = 0, height = 0),
                     size = 2, vjust = 1)
}

## hack_boxplot(x = "UNSD_MACRO_REG", y = "NY.GNP.PCAP.CD", year = 2010,
##              data = box.df, col = plot_colors(n = 6)$Sub)



## with(plotInfo,
##    {
##     print(hack_boxplot(x = "UNSD_MACRO_REG", y = yAxis, year = plotYears,
##                  scale = scaling, x_lab = xPlotLab, y_lab = yPlotLab,
##                  col = plot_colors(n = 6)$Sub, data = box.df))
## })
