#overall exploration of nominal and text vars
odf <- readRDS("uwlpuq.rds")

#other: text responses (wetlands section)
unique(odf$wlknot);table(odf$wlknot)
unique(odf$wlanot);table(odf$wlanot)
unique(odf$wlplot);table(odf$wlplot)
unique(odf$wlthot);table(odf$wlthot)
#check categorical politomous vars
unique(odf$wlrspn);table(odf$wlrspn)

#other: text responses (maria behety and tres puentes)
unique(odf$mbacot);table(odf$mbacot)
unique(odf$mbtrot);table(odf$mbtrot)
unique(odf$mbprot);table(odf$mbprot)
unique(odf$tpacot);table(odf$tpacot)
unique(odf$tptrot);table(odf$tptrot)
unique(odf$tpprot);table(odf$tpprot)
unique(odf$mbnvot);table(odf$mbnvot)
unique(odf$tpnvot);table(odf$tpnvot)
#check categorical politomous vars (maria behety and tres puentes)
unique(odf$mbnvis);table(odf$mbnvis)
unique(odf$tpnvis);table(odf$tpnvis)
unique(odf$mbtran);table(odf$mbtran)
unique(odf$tptran);table(odf$tptran)
unique(odf$mbbemi);table(odf$mbbemi)
unique(odf$tpbemi);table(odf$tpbemi)

#other: text responses (demog section)
unique(odf$natiot);table(odf$natiot)
unique(odf$bplaot);table(odf$bplaot)
unique(odf$ethnot);table(odf$ethnot)
unique(odf$occupa);table(odf$occupa)

#check "encuestadores"
unique(odf$encnam);table(odf$encnam)