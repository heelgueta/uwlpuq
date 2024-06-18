#overall exploration of open text vars
odf <- readRDS("uwlpuq.rds")

#other: text responses (wetlands section)
unique(odf$wlknot);table(odf$wlknot)
unique(odf$wlanot);table(odf$wlanot)
unique(odf$wlplot);table(odf$wlplot)
unique(odf$wlthot);table(odf$wlthot)

#other: text responses (maria behety and tres puentes)
unique(odf$mbacot);table(odf$mbacot)
unique(odf$mbtrot);table(odf$mbtrot)
unique(odf$mbprot);table(odf$mbprot)
unique(odf$tpacot);table(odf$tpacot)
unique(odf$tptrot);table(odf$tptrot)
unique(odf$tpprot);table(odf$tpprot)
unique(odf$mbnvot);table(odf$mbnvot)
unique(odf$tpnvot);table(odf$tpnvot)

