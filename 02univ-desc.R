#load data, setup
options(digits = 3)
options(scipen = 999)
options(max.print = 5000)
odf <- readRDS("uwlpuq.rds")
#install.packages("psych")

#univariate analyses for ordinal vars
data.frame(colnames(odf))
#general env attitudes and behaviors
psych::describe(odf[,c(2,8:33)])
#
#
#univariate analyses for dichotomous vars
#wetlands in general
psych::describe(odf[,c(34:40,42:68,70:76,78:91)])[,c(1,4)] #% of mentions

