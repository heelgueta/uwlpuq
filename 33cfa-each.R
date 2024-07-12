options(digits = 3);options(scipen = 999);options(max.print = 5000)
odf <- readRDS("uwlpuq.rds");idf <- readRDS("uwlpui.rds")

library(lavaan)

#bint, second model nearly good
mod <- 'bint =~ bint01 + bint02 + bint03 + bint04 + bint05 + bint06 + bint07 + bint08 + bint09 + bint10'

#good
mod <- 'bintf =~ bint01 + bint02 + bint03 + bint04 + bint05 + bint06 + bint07 + bint08'# + bint09 + bint10
mod <- 'wlprf =~ wlpri1 + wlpri2 + wlpri3 + wlpri4'



mod <- 'regif =~ regid1 + regid2 + regid3 + regid4'



mod <- 'platf =~ place1 + place2 + place3 + place4'
mod <- 'natcf =~ natcn1 + natcn2 + natcn3 + natcn4'
mod <- 'penvf =~ penvb1 + penvb2 + penvb3 + penvb4 + penvb5 + penvb6'


#not good
mod <- 'envec =~ envec1 + envec2 + envec3 + envec4 + envec5 + envec6'
mod <- 'pecsa =~ pecsa1 + pecsa2 + pecsa3'
mod <- 'envpa =~ envpa1 + envpa2 + envpa3 + envpa4 + envpa5 + envpa6 + envpa7'
mod <- 'mbtpv =~ mbfreq + mbleng + mblvis + tpfreq + tpleng + tplvis'
mod <- 'mbtpv =~ mbfreq + mbleng + tpfreq + tpleng;tpfreq ~~ tpleng;mbfreq ~~ mbleng'




mod <- 'biocef =~ wlim01 + wlim02 + wlim03 + wlim04'
mod <- 'humanf =~ wlim06 + wlim07 + wlim08 + wlim09'
mod <- 'biocef =~ wlim01 + wlim02 + wlim03 + wlim04
        humanf =~ wlim06 + wlim07 + wlim08 + wlim09'


# Run the CFA
fit <- cfa(mod, data = idf,estimator = "DWLS",ordered = T)

summary(fit)


fitMeasures(fit, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))
standardizedSolution(fit)


#standardizedsolution(fit)[standardizedsolution(fit)$op=='=~',c(1:4,7)]
standardizedsolution(fit)[standardizedsolution(fit)$op=='=~',c(1:4,7)][order(-abs(standardizedsolution(fit)[standardizedsolution(fit)$op=='=~',c(1:4,7)][, 4])),]
hist(predict(fit)[,1])
hist(predict(fit)[,2])
predict(fit)
length(unique(predict(fit)[,1]))
unique(predict(fit)[,2])
