#univariate exploratory/descriptive analyses of numerical vars

#setup
options(digits = 3);options(scipen = 999);options(max.print = 5000)#quick setup
odf <- readRDS("uwlpuq.rds");idf <- readRDS("uwlpui.rds")#load datasets if needed
#install.packages("psych")#if needed

psych::describe(odf[,c(214,221,222,223)])
