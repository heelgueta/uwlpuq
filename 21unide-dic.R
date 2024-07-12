###########################################################
# 21setup.R                                               #
# 2.1 Univariate descriptive analyses for dichotomous vars#
###########################################################

options(digits = 3);options(scipen = 999);options(max.print = 5000)
odf <- readRDS("uwlpuq.rds")
colnames(odf)
#install.packages("psych")

#wetlands in general


psych::describe(odf[,c(34:40)])

[,c(2,3)]



cbind(psych::describe(odf[,c(34:40)]), t(sapply(odf[, c(34:40)], function(x) table(factor(x, levels = 0:1, exclude = NULL)))))[,c(2,3,14:15)]


psych::describe(odf[,c(34:40)])[,c(2,3)] #% of mentions #knows what wls?
psych::describe(odf[,c(42:56)])[,c(2,3)] #% of mentions #importance of wls?
psych::describe(odf[,c(57:68)])[,c(2,3)] #% of mentions #animals
psych::describe(odf[,c(70:76)])[,c(2,3)] #% of mentions #plants
psych::describe(odf[,c(78:91)])[,c(2,3)] #% of mentions #threats

#maria behety 
psych::describe(odf[,c(100:107)])[,c(2,3)] #% of mentions #activities
psych::describe(odf[,c(114:125)])[,c(2,3)] #% of mentions #benefits
psych::describe(odf[,c(129:136)])[,c(2,3)] #% of mentions #problems

#tres puentes
psych::describe(odf[,c(143:150)])[,c(2,3)] #% of mentions #activities
psych::describe(odf[,c(157:168)])[,c(2,3)] #% of mentions #benefits
psych::describe(odf[,c(172:181)])[,c(2,3)] #% of mentions #problems

#OR
vars <- c("wlknw1", "wlknw2", "wlknw3", "wlknw4", "wlknw5", "wlknw6", "wlknw7", 
          "wlim01", "wlim02", "wlim03", "wlim04", "wlim05", "wlim06", "wlim07", 
          "wlim08", "wlim09", "wlim10", "wlim11", "wlim12", "wlim13", "wlim14", 
          "wlim15", "wlan01", "wlan02", "wlan03", "wlan04", "wlan05", "wlan06", 
          "wlan07", "wlan08", "wlan09", "wlan10", "wlan11", "wlan12", "wlpl01", 
          "wlpl02", "wlpl03", "wlpl04", "wlpl05", "wlpl06", "wlpl07", "wlth01", 
          "wlth02", "wlth03", "wlth04", "wlth05", "wlth06", "wlth07", "wlth08", 
          "wlth09", "wlth10", "wlth11", "wlth12", "wlth13", "wlth14", "mbac01", 
          "mbac02", "mbac03", "mbac04", "mbac05", "mbac06", "mbac07", "mbac08", 
          "mbbe01", "mbbe02", "mbbe03", "mbbe04", "mbbe05", "mbbe06", "mbbe07", 
          "mbbe08", "mbbe09", "mbbe10", "mbbe11", "mbbe12", "mbpr01", "mbpr02", 
          "mbpr03", "mbpr04", "mbpr05", "mbpr06", "mbpr07", "mbpr08", "tpac01", 
          "tpac02", "tpac03", "tpac04", "tpac05", "tpac06", "tpac07", "tpac08", 
          "tpbe01", "tpbe02", "tpbe03", "tpbe04", "tpbe05", "tpbe06", "tpbe07", 
          "tpbe08", "tpbe09", "tpbe10", "tpbe11", "tpbe12", "tppr01", "tppr02", 
          "tppr03", "tppr04", "tppr05", "tppr06", "tppr07", "tppr08", "tppr09", 
          "tppr10")

for (var in vars) {
  if (var %in% names(odf)) {
    cat("\n", var, "\n", sep="")
    print(table(odf[[var]]))
  } else {
    cat("\n", var, " not found in the dataframe\n", sep="")
  }
}




cbind(psych::describe(odf[,c(34:40, 42:56, 57:68, 70:76, 78:91, 100:107, 114:125, 129:136, 143:150, 157:168, 172:181)]), 
      t(sapply(odf[, c(34:40, 42:56, 57:68, 70:76, 78:91, 100:107, 114:125, 129:136, 143:150, 157:168, 172:181)], 
               function(x) table(factor(x, levels = 0:1, exclude = NULL)))))[,c(2,3,14:15)]


cbind(cbind(psych::describe(odf[,c(34:40, 42:56, 57:68, 70:76, 78:91, 100:107, 114:125, 129:136, 143:150, 157:168, 172:181)]), 
            t(sapply(odf[, c(34:40, 42:56, 57:68, 70:76, 78:91, 100:107, 114:125, 129:136, 143:150, 157:168, 172:181)], 
                     function(x) table(factor(x, levels = 0:1, exclude = NULL)))))[,c(2,3,14:15)], 
      vardesc = read.csv("vardesc.csv", header = TRUE)[match(rownames(psych::describe(odf[,c(34:40, 42:56, 57:68, 70:76, 78:91, 100:107, 114:125, 129:136, 143:150, 157:168, 172:181)])), read.csv("vardesc.csv", header = TRUE)$varname), "vardesc"])


library(writexl)

result <- cbind(cbind(psych::describe(odf[,c(34:40, 42:56, 57:68, 70:76, 78:91, 100:107, 114:125, 129:136, 143:150, 157:168, 172:181)]), 
                      t(sapply(odf[, c(34:40, 42:56, 57:68, 70:76, 78:91, 100:107, 114:125, 129:136, 143:150, 157:168, 172:181)], 
                               function(x) table(factor(x, levels = 0:1, exclude = NULL)))))[,c(2,3,14:15)], 
                vardesc = read.csv("vardesc.csv", header = TRUE)[match(rownames(psych::describe(odf[,c(34:40, 42:56, 57:68, 70:76, 78:91, 100:107, 114:125, 129:136, 143:150, 157:168, 172:181)])), read.csv("vardesc.csv", header = TRUE)$varname), "vardesc"])

write_xlsx(as.data.frame(result), "output_table.xlsx")



library(writexl)

write_xlsx(
  data.frame(
    Variable = rownames(
      psych::describe(odf[,c(34:40, 42:56, 57:68, 70:76, 78:91, 100:107, 114:125, 129:136, 143:150, 157:168, 172:181)])
    ),
    cbind(
      psych::describe(odf[,c(34:40, 42:56, 57:68, 70:76, 78:91, 100:107, 114:125, 129:136, 143:150, 157:168, 172:181)]),
      t(sapply(odf[, c(34:40, 42:56, 57:68, 70:76, 78:91, 100:107, 114:125, 129:136, 143:150, 157:168, 172:181)], 
               function(x) table(factor(x, levels = 0:1, exclude = NULL))))
    )[,c(2,3,14:15)],
    vardesc = read.csv("vardesc.csv", header = TRUE)[match(
      rownames(psych::describe(odf[,c(34:40, 42:56, 57:68, 70:76, 78:91, 100:107, 114:125, 129:136, 143:150, 157:168, 172:181)])),
      read.csv("vardesc.csv", header = TRUE)$varname
    ), "vardesc"]
  ),
  "output_table.xlsx"
)
