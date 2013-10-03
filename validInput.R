##' Check the inputs
##'
##' A function for checking whether the dimension and the composition of
##' the metadata, construction and the dissemination file satisfy the
##' definition.
##'
##' @param paramFile The parameter file.
##' @param constFile The construction file.
##' @param dissFile The dissemination file.

validInput = function(paramFile, constFile, dissFile){
  printLab("Testing input file")
  failList = list()
  paramRawKey = as.vector(unlist(subset(paramFile, select = ("DATA_KEY"),
                                         subset = DATA_TYPE == "raw")))
  paramProcessKey = as.vector(unlist(subset(paramFile, select = ("DATA_KEY"),
                                             subset = DATA_TYPE == "processed")))
  constructRawKey = as.vector(na.omit(unlist(subset(constFile,
                                           select = c("DATA_KEY_ORIG1",
                                             "DATA_KEY_ORIG2")))))
  constructProcessKey = as.vector(na.omit(unlist(subset(constFile,
                                               select = c("DATA_KEY_PROC"),
                                   subset = CONSTRUCTION_TYPE != "agg"))))
  dissKey = as.vector(na.omit(unlist(subset(dissFile,
                               select = c("DATA_KEY1", "DATA_KEY2")))))

  cat("Test1: Do all the objects in dissemination file have the required fields filled? \n")
  cat("Required fields: PART_CHAPTER_NUMBER, PART, TOPIC_NAME_ABB,
      SPREAD_NUMBERorTABLE_NUMBER, COLUMN_NUMBERorOBJECT_ORDER,
      YearStart, YearEnd, ORIG_UNIT, NUMBER_OF_VARIABLES, DATA_KEY1\n...")
  required.vars = subset(dissFile,
                          select = c("PART_CHAPTER_NUMBER",
                                     "PART",
                                     "TOPIC_NAME_ABB",
                                     "SPREAD_NUMBERorTABLE_NUMBER",
                                     "COLUMN_NUMBERorOBJECT_ORDER",
                                     "OBJECT",
                                     "COLUMN_NAMEorCAPTION",
                                     "YearStart",
                                     "YearEnd",
                                     "ORIG_UNIT",
                                     "NUMBER_OF_VARIABLES",
                                     "DATA_KEY1"))
  required.na = na.omit(required.vars)
  required.vars.miss = as.numeric(attr(required.na, "na.action"))
  cat(paste(ifelse(length(required.vars.miss) == 0, TRUE, FALSE)), "\n\n", sep = "")
  failList$test1 = required.vars[required.vars.miss,
                                        c("PART_CHAPTER_NUMBER","DATA_KEY1")]

  cat("Test2: Do charts and maps have TOPIC_NAME & TeX_ENVIRONMENT filled? ... ")
  required.vars1 = subset(dissFile,
                           select = c("PART_CHAPTER_NUMBER",
                                      "TOPIC_NAME",
                                      "TeX_ENVIRONMENT"),
                           subset = OBJECT %in% c("CHART", "MAP"))
  required.na1 = na.omit(required.vars1)
  required.vars1.miss = as.numeric(attr(required.na1, "na.action"))
  cat(paste(ifelse(length(required.vars1.miss) == 0, TRUE, FALSE)), "\n\n", sep = "")
  failList$test2 = required.vars1[required.vars1.miss, "PART_CHAPTER_NUMBER"]

  cat("Test3: Is PART between 1:6, SPREAD_NUMBERorTABLE_NUMBER numeric,
      COLUMN_NUMBERorOBJECT_ORDER numeric, TeX_ENVIRONMENT between 1:6,
      OBJECT in CHART or MAP or TABLE or MINITABLE? ... ")
  wrong.part = subset(dissFile,
                       select = "PART_CHAPTER_NUMBER",
                       subset = !PART %in% c(1:6))
  wrong.sprd = subset(dissFile,
                       select = "PART_CHAPTER_NUMBER",
                       subset = !is.numeric(SPREAD_NUMBERorTABLE_NUMBER))
  wrong.clnm = subset(dissFile,
                       select = "PART_CHAPTER_NUMBER",
                       subset = !is.numeric(COLUMN_NUMBERorOBJECT_ORDER))
  wrong.texe = subset(dissFile,
                       select = "PART_CHAPTER_NUMBER",
                       subset = OBJECT!="TABLE" & !TeX_ENVIRONMENT %in% c(1:6))
  wrong.objt = subset(dissFile,
                       select = "PART_CHAPTER_NUMBER",
                       subset = !OBJECT %in% c("CHART", "MAP", "TABLE", "MINITABLE"))
  wrong.descr = c(unlist(wrong.part),
                   unlist(wrong.sprd),
                   unlist(wrong.clnm),
                   unlist(wrong.texe),
                   unlist(wrong.objt))
  cat(paste(ifelse(length(wrong.descr) == 0, TRUE, FALSE), "\n\n", sep = ""))
  failList$test3 = wrong.descr

  cat("Test4: PART_CHAPTER_NUMBER well written? ... ")
  ready.vars = na.omit(subset(dissFile,
                               select = c("PART_CHAPTER_NUMBER",
                                          "OBJECT",
                                          "PART",
                                          "TOPIC_NAME_ABB",
                                          "SPREAD_NUMBERorTABLE_NUMBER",
                                          "COLUMN_NUMBERorOBJECT_ORDER")))
  wrong.name = c()
  for (i in 1:nrow(ready.vars)) {
    if (ready.vars[i,"OBJECT"]=="CHART") {
      tob = "C"
    } else if (ready.vars[i,"OBJECT"]=="MAP") {
      tob = "M"
    } else if (ready.vars[i,"OBJECT"]=="TABLE") {
      tob = "T"
    } else if (ready.vars[i,"OBJECT"]=="MINITABLE") {
      tob = "MT"
    }

    if (!ready.vars[i,"OBJECT"]=="TABLE") {
      name.created = paste(tob, "_P", ready.vars[i,"PART"],
                            ".", ready.vars[i,"TOPIC_NAME_ABB"], ".",
                            ready.vars[i,"SPREAD_NUMBERorTABLE_NUMBER"], ".",
                            ready.vars[i,"COLUMN_NUMBERorOBJECT_ORDER"], sep = "")
    } else {
      name.created = paste(tob, "_P", ready.vars[i,"PART"],
                            ".", ready.vars[i,"TOPIC_NAME_ABB"], ".",
                            ready.vars[i,"SPREAD_NUMBERorTABLE_NUMBER"], sep = "")
    }

    if (!name.created==ready.vars[i,"PART_CHAPTER_NUMBER"]) wrong.name = c(wrong.name, i)
  }
  cat(paste(ifelse(is.null(wrong.name), TRUE, FALSE)), "\n\n", sep = "")
  failList$test4 = ready.vars[wrong.name, "PART_CHAPTER_NUMBER"]

  cat("Test5: Is PLOT_OBJECTIVE in Compare or Display or Decompose and is consistent
      with the number of variables? ... ")
  wrong.obj = subset(dissFile, select = "PART_CHAPTER_NUMBER",
                      subset = !PLOT_OBJECTIVE %in% c("Display", "Compare", "Decompose") &
                        PLOT_OBJECTIVE != "" & MANUAL != "manual")
  wrongDispType = subset(dissFile, select = "PART_CHAPTER_NUMBER",
                          subset = PLOT_OBJECTIVE == "Display" &
                            NUMBER_OF_VARIABLES != 1 & MANUAL != "manual")
  wrongCompType = subset(dissFile, select = "PART_CHAPTER_NUMBER",
                          subset = PLOT_OBJECTIVE != "Display" &
                            NUMBER_OF_VARIABLES < 2 & MANUAL != "manual")
  wrongType = c(unlist(wrong.obj),unlist(wrongDispType), unlist(wrongCompType))
  cat(paste(ifelse(length(wrongType) == 0, TRUE, FALSE), "\n\n", sep = ""))
  failList$test5 = wrongType

  cat("Test6: Is TYPE = small or big if it is a map? ... ")
  wrong.type = subset(dissFile, select = "PART_CHAPTER_NUMBER",
                       subset = !TYPE %in% c("small", "big") &
                        OBJECT == "MAP")
  wrong.type = unlist(wrong.type)
  cat(paste(ifelse(length(wrong.type) == 0, TRUE, FALSE), "\n\n", sep = ""))
  failList$test6 = wrong.type

  cat("Test7: Have maps and tables the number of variable = 1,
      thus the right number of datakeys? ... ")
  wrong.numb = subset(dissFile,
                       select = "PART_CHAPTER_NUMBER",
                       subset = OBJECT %in% c("TABLE", "MAP") &
                         NUMBER_OF_VARIABLES != 1)
  wrong.numb.key = subset(dissFile,
                           select = "PART_CHAPTER_NUMBER",
                           subset = OBJECT %in% c("TABLE", "MAP") & !DATA_KEY2=="")
  wrong.numb.vars = c(unlist(wrong.numb), unlist(wrong.numb.key))
  cat(paste(ifelse(length(wrong.numb.vars) == 0, TRUE, FALSE), "\n\n", sep = ""))
  failList$test7 = wrong.numb.vars

  cat("Test8: Is QUANTITY in dissemination file in
      unit, thousand, million, billion, trillion? ... ")
  wrong.qnty = subset(dissFile,
                       select = c("PART_CHAPTER_NUMBER", "QUANTITY"),
                       subset = !QUANTITY %in%
                         c("unit", "thousand", "million", "billion", "trillion")
                       & !QUANTITY=="")
  wrong.qnty = unlist(wrong.qnty)
  cat(paste(ifelse(length(wrong.qnty) == 0, TRUE, FALSE), "\n\n", sep = ""))
  failList$test8 = wrong.qnty

  cat("Test9: Are YearStart and YearEnd numeric, and YearStart < YearEnd? ... ")
  wrong.yr = subset(dissFile,
                     select = c("PART_CHAPTER_NUMBER", "YearStart", "YearEnd"),
                     subset = !is.numeric(YearStart)|!is.numeric(YearEnd)|
                       YearStart > YearEnd)
  wrong.yr = unlist(wrong.yr)
  cat(paste(ifelse(length(wrong.yr) == 0, TRUE, FALSE), "\n\n", sep = ""))
  failList$test9 = wrong.yr

  cat("Test10: Do all the objects in metadata file have the required fields filled? \n")
  cat("Required fields: DATA_KEY, SERIES_NAME, SERIES_NAME_SHORT,
      ORIG_UNIT, OWNER, SOURCE, DATA_TYPE\n...")
  required.vars = subset(paramFile,
                          select = c("DATA_KEY",
                                     "SERIES_NAME",
                                     "SERIES_NAME_SHORT",
                                     "ORIG_UNIT",
                                     "OWNER",
                                     "SOURCE",
                                     "DATA_TYPE"))
  required.na = na.omit(required.vars)
  required.vars.miss = as.numeric(attr(required.na, "na.action"))
  cat(paste(ifelse(length(required.vars.miss) == 0, TRUE, FALSE)), "\n\n", sep = "")
  failList$test10 = required.vars[required.vars.miss, "DATA_KEY"]

  cat("Test11: Is DATA_KEY in metadata file unique? ... ")
  dupkey = duplicated(paramFile$DATA_KEY)
  cat(paste(!any(dupkey), "\n\n", sep = ""))
  failList$test11 = paramFile[dupkey, "DATA_KEY"]

  cat("Test12: Is QUANTITY in metadata file in
      unit, thousand, million, billion, trillion? ... ")
  wrong.qnty = subset(paramFile,
                       select = c("DATA_KEY", "QUANTITY"),
                       subset = !QUANTITY %in%
                         c("unit", "thousand", "million", "billion", "trillion")
                       & !QUANTITY=="")
  wrong.qnty = unlist(wrong.qnty)
  cat(paste(ifelse(length(wrong.qnty) == 0, TRUE, FALSE), "\n\n", sep = ""))
  failList$test12 = wrong.qnty

  cat("Test13: Is DATA_TYPE in metadata file in raw or processed? ... ")
  wrong.type = subset(paramFile,
                       select = c("DATA_KEY", "DATA_TYPE"),
                       subset = !DATA_TYPE %in% c("raw", "processed"))
  wrong.type = unlist(wrong.type)
  cat(paste(ifelse(length(wrong.type) == 0, TRUE, FALSE), "\n\n", sep = ""))
  failList$test13 = wrong.type

  cat("Test14: Is WDINAME in metadata file unique? ... ")
  wdi.vars = subset(paramFile, subset = WDINAME!= "")
  dupwdi = duplicated(wdi.vars$WDINAME)
  cat(paste(!any(dupwdi), "\n\n", sep = ""))
  failList$test14 = paramFile[dupwdi, "DATA_KEY"]

  cat("Test15: All SQL queries are filled in? ... ")
  sql.df = subset(paramFile,
                   select = c("DATA_KEY", "SQL_DOMAIN_CODE",
                              "SQL_ELEMENT_CODE", "SQL_ITEM_CODE"),
                   subset = DATA_TYPE == "raw" & SQL!= "" & !is.na(SQL))
  sql.na = na.omit(sql.df)
  sqlMiss = as.numeric(attr(sql.na, "na.action"))
  cat(paste(ifelse(length(sqlMiss) == 0, TRUE, FALSE)), "\n\n", sep = "")
  failList$test15 = sql.df[sqlMiss, "DATA_KEY"]

  cat("Test16: All raw data have INFO filled? ... ")
  info.df = subset(paramFile,
                    select = c("DATA_KEY", "INFO"),
                    subset = DATA_TYPE == "raw")
  info.na = na.omit(info.df)
  infoMiss = as.numeric(attr(info.na, "na.action"))
  cat(paste(ifelse(length(infoMiss) == 0, TRUE, FALSE)), "\n\n", sep = "")
  failList$test16 = info.df[infoMiss, "DATA_KEY"]


  cat("Test17: Do all the objects in construction file have the required fields filled?\n")
  cat("Required fields: DATA_KEY_PROC, DATA_KEY_ORIG1, CONSTRUCTION_TYPE\n...")
  required.vars = subset(constFile,
                          select = c("DATA_KEY_PROC",
                                     "DATA_KEY_ORIG1",
                                     "CONSTRUCTION_TYPE"))
  required.na = na.omit(required.vars)
  required.vars.miss = as.numeric(attr(required.na, "na.action"))
  cat(paste(ifelse(length(required.vars.miss) == 0, TRUE, FALSE)), "\n\n", sep = "")
  failList$test17 = required.vars[required.vars.miss, "DATA_KEY"]

  cat("Test18: Is DATA_KEY_PROC in construction file unique? ... ")
  dupkey = duplicated(constFile$DATA_KEY_PROC)
  cat(paste(!any(dupkey), "\n\n", sep = ""))
  failList$test18 = constFile[dupkey, "DATA_KEY_PROC"]

  cat("Test19: Is CONSTRUCTION_TYPE in construction file in
      agg, growth, share, change, specific, manual? ... ")
  wrong.type = subset(constFile,
                       select = c("DATA_KEY_PROC", "CONSTRUCTION_TYPE"),
                       subset = !CONSTRUCTION_TYPE %in%
                         c("agg", "growth", "share", "change", "specific", "manual"))
  wrong.type = unlist(wrong.type)
  cat(paste(ifelse(length(wrong.type) == 0, TRUE, FALSE), "\n\n", sep = ""))
  failList$test19 = wrong.type

  cat("Test20: All growth rate frequency filled in the construction file? ... ")
  grFreqdf = subset(constFile, select = "DATA_KEY_PROC",
                       subset = CONSTRUCTION_TYPE == "growth")
  grFreqMiss = is.na(subset(constFile, select = "GROWTH_RATE_FREQ",
                             subset = CONSTRUCTION_TYPE == "growth"))
  cat(paste(all(!grFreqMiss), "\n\n", sep = ""))
  failList$test20 = grFreqdf[grFreqMiss, "DATA_KEY_PROC"]

  cat("Test21: All raw data required in the construction are in the metadata? ... ")
  constRawInParam = constructRawKey %in% paramRawKey
  cat(paste((all(constRawInParam)), "\n\n", sep = ""))
  failList$test21 = constructRawKey[!constRawInParam]

  ## For the following test we already know that DATA_KEY & DATA_KEY_PROC
  ## are unique
  cat("Test22: 1 to 1 correspondence between DATA_KEY variables in metadata
      and DATA_KEY_PROC in construction? ... ")
  meta.key = paramFile[, "DATA_KEY"]
  const.key = constFile[, "DATA_KEY_PROC"]
  mis.match1 = subset(meta.key, !meta.key%in%const.key)
  mis.match2 = subset(const.key, !const.key%in%meta.key)
  cat(paste(!any(c(mis.match1, mis.match2))), "\n\n", sep = "")
  failList$test22 = c(mis.match1,mis.match2)

  cat("Test23: Have all data keys in dissemination file a
      correspondent variable in metadata? ... ")
  dissInParam = dissKey %in% c(paramRawKey, paramProcessKey)
  cat(paste((all(dissInParam)), "\n\n", sep = ""))
  failList$test23 = dissKey[!dissInParam]

  cat("Test24: All chart supported? ... ")
  unSupported = unlist(subset(dissFile, select = "PART_CHAPTER_NUMBER",
                               subset = AREA != "World" & YearStart != YearEnd &
                               NUMBER_OF_VARIABLES > 1 & MANUAL != "manual"))
  cat(paste(ifelse(length(unSupported) == 0, TRUE, FALSE), "\n\n", sep = ""))
  failList$test24 = unSupported

  cat("Test25: Units in the meta data matched with units in the dissemination? ... ")
  d0 = subset(paramFile, select = c("DATA_KEY", "ORIG_UNIT"))
  colnames(d0) = c("DATA_KEY", "ORIG_UNIT")
  d1 = subset(dissFile, select = c("DATA_KEY1", "ORIG_UNIT"))
  colnames(d1) = c("DATA_KEY", "ORIG_UNIT_D1")
  d2 = subset(dissFile, select = c("DATA_KEY2", "ORIG_UNIT"),
               subset = !is.na(DATA_KEY2))
  colnames(d2) = c("DATA_KEY", "ORIG_UNIT_D2")

  ## This must come after check all the disem variable are in the metadata
  d1Comp = merge(d0, d1, all.y = TRUE)
  d2Comp = merge(d0, d2, all.y = TRUE)
  d1MisMatch = d1Comp$ORIG_UNIT != d1Comp$ORIG_UNIT_D1
  d2MisMatch = d2Comp$ORIG_UNIT != d2Comp$ORIG_UNIT_D2

  cat(paste(!any(c(d1MisMatch, d1MisMatch))), "\n\n", sep = "")
  failList$test25 = list(d1Mismatch = d1Comp[d1MisMatch, ],
                          d2Mismatch = d2Comp[d1MisMatch, ])

  failList
}
