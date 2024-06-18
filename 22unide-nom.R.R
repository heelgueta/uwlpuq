##################################################################
# 22unide-nom.R                                                  #
# 2.2 Univariate descriptive analyses for politomous nominal vars#
##################################################################

#check categorical politomous vars
unique(odf$wlrspn);table(odf$wlrspn)

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