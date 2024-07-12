
#list of vars
print(read.csv("vardesc.csv", stringsAsFactors = FALSE)[match(c("wlknw1", "wlknw2", "wlknw3", "wlknw4", "wlknw5", "wlknw6", "wlknw7", "wlim01", "wlim02", "wlim03", "wlim04", "wlim05", "wlim06", "wlim07", "wlim08", "wlim09", "wlim10", "wlim11", "wlim12", "wlim13", "wlim14", "wlim15", "wlan01", "wlan02", "wlan03", "wlan04", "wlan05", "wlan06", "wlan07", "wlan08", "wlan09", "wlan10", "wlan11", "wlan12", "wlpl01", "wlpl02", "wlpl03", "wlpl04", "wlpl05", "wlpl06", "wlpl07", "wlth01", "wlth02", "wlth03", "wlth04", "wlth05", "wlth06", "wlth07", "wlth08", "wlth09", "wlth10", "wlth11", "wlth12", "wlth13", "wlth14", "mbac01", "mbac02", "mbac03", "mbac04", "mbac05", "mbac06", "mbac07", "mbac08", "mbbe01", "mbbe02", "mbbe03", "mbbe04", "mbbe05", "mbbe06", "mbbe07", "mbbe08", "mbbe09", "mbbe10", "mbbe11", "mbbe12", "mbpr01", "mbpr02", "mbpr03", "mbpr04", "mbpr05", "mbpr06", "mbpr07", "mbpr08", "tpac01", "tpac02", "tpac03", "tpac04", "tpac05", "tpac06", "tpac07", "tpac08", "tpbe01", "tpbe02", "tpbe03", "tpbe04", "tpbe05", "tpbe06", "tpbe07", "tpbe08", "tpbe09", "tpbe10", "tpbe11", "tpbe12", "tppr01", "tppr02", "tppr03", "tppr04", "tppr05", "tppr06", "tppr07", "tppr08", "tppr09", "tppr10"), read.csv("vardesc.csv", stringsAsFactors = FALSE)$varname), "vardesc"])
cat(print(read.csv("vardesc.csv", stringsAsFactors = FALSE)[match(c("wlknw1", "wlknw2", "wlknw3", "wlknw4", "wlknw5", "wlknw6", "wlknw7", "wlim01", "wlim02", "wlim03", "wlim04", "wlim05", "wlim06", "wlim07", "wlim08", "wlim09", "wlim10", "wlim11", "wlim12", "wlim13", "wlim14", "wlim15", "wlan01", "wlan02", "wlan03", "wlan04", "wlan05", "wlan06", "wlan07", "wlan08", "wlan09", "wlan10", "wlan11", "wlan12", "wlpl01", "wlpl02", "wlpl03", "wlpl04", "wlpl05", "wlpl06", "wlpl07", "wlth01", "wlth02", "wlth03", "wlth04", "wlth05", "wlth06", "wlth07", "wlth08", "wlth09", "wlth10", "wlth11", "wlth12", "wlth13", "wlth14", "mbac01", "mbac02", "mbac03", "mbac04", "mbac05", "mbac06", "mbac07", "mbac08", "mbbe01", "mbbe02", "mbbe03", "mbbe04", "mbbe05", "mbbe06", "mbbe07", "mbbe08", "mbbe09", "mbbe10", "mbbe11", "mbbe12", "mbpr01", "mbpr02", "mbpr03", "mbpr04", "mbpr05", "mbpr06", "mbpr07", "mbpr08", "tpac01", "tpac02", "tpac03", "tpac04", "tpac05", "tpac06", "tpac07", "tpac08", "tpbe01", "tpbe02", "tpbe03", "tpbe04", "tpbe05", "tpbe06", "tpbe07", "tpbe08", "tpbe09", "tpbe10", "tpbe11", "tpbe12", "tppr01", "tppr02", "tppr03", "tppr04", "tppr05", "tppr06", "tppr07", "tppr08", "tppr09", "tppr10"), read.csv("vardesc.csv", stringsAsFactors = FALSE)$varname), "vardesc"]),sep="\n")

# Extend the dataframe idf with merged variables
idf <- idf %>%
  mutate(
    wlac01 = ifelse(tpac01 + mbac01 == 0, 0, 1),
    wlac02 = ifelse(tpac02 + mbac02 == 0, 0, 1),
    wlac03 = ifelse(tpac03 + mbac03 == 0, 0, 1),
    wlac04 = ifelse(tpac04 + mbac04 == 0, 0, 1),
    wlac05 = ifelse(tpac05 + mbac05 == 0, 0, 1),
    wlac06 = ifelse(tpac06 + mbac06 == 0, 0, 1),
    wlac07 = ifelse(tpac07 + mbac07 == 0, 0, 1),
    wlac08 = ifelse(tpac08 + mbac08 == 0, 0, 1),
    wlac09 = ifelse(tpac09 + mbac09 == 0, 0, 1)
  )

idf <- idf %>%
  mutate(
    wlbe01 = ifelse(tpbe01 + mbbe01 == 0, 0, 1),
    wlbe02 = ifelse(tpbe02 + mbbe02 == 0, 0, 1),
    wlbe03 = ifelse(tpbe03 + mbbe03 == 0, 0, 1),
    wlbe04 = ifelse(tpbe04 + mbbe04 == 0, 0, 1),
    wlbe05 = ifelse(tpbe05 + mbbe05 == 0, 0, 1),
    wlbe06 = ifelse(tpbe06 + mbbe06 == 0, 0, 1),
    wlbe07 = ifelse(tpbe07 + mbbe07 == 0, 0, 1),
    wlbe08 = ifelse(tpbe08 + mbbe08 == 0, 0, 1),
    wlbe09 = ifelse(tpbe09 + mbbe09 == 0, 0, 1),
    wlbe10 = ifelse(tpbe10 + mbbe10 == 0, 0, 1),
    wlbe11 = ifelse(tpbe11 + mbbe11 == 0, 0, 1),
    wlbe12 = ifelse(tpbe12 + mbbe12 == 0, 0, 1)
  )

idf <- idf %>%
  mutate(
    wlpr01 = ifelse(tppr01 + mbpr01 == 0, 0, 1),
    wlpr02 = ifelse(tppr02 + mbpr02 == 0, 0, 1),
    wlpr03 = ifelse(tppr03 + mbpr03 == 0, 0, 1),
    wlpr04 = ifelse(tppr04 + mbpr04 == 0, 0, 1),
    wlpr05 = ifelse(tppr05 + mbpr05 == 0, 0, 1),
    wlpr06 = ifelse(tppr06 + mbpr06 == 0, 0, 1),
    wlpr07 = ifelse(tppr07 + mbpr07 == 0, 0, 1),
    wlpr08 = ifelse(tppr08 + mbpr08 == 0, 0, 1)
  )
# Save the new dataframe as an RDS file
saveRDS(idf, file = "uwlpue.rds")
#extended imputed dataframe
edf <- readRDS("uwlpue.rds")
idf <- readRDS("uwlpui.rds")




