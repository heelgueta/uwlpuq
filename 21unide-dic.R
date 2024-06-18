###########################################################
# 21setup.R                                               #
# 2.1 Univariate descriptive analyses for dichotomous vars#
###########################################################

options(digits = 3);options(scipen = 999);options(max.print = 5000)
odf <- readRDS("uwlpuq.rds")

#install.packages("psych")

#wetlands in general
psych::describe(odf[,c(34:40)])[,c(1,3)] #% of mentions
psych::describe(odf[,c(34:40,42:68,70:76,78:91)])[,c(1,3)] #% of mentions