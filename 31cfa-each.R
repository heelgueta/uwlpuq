options(digits = 3);options(scipen = 999);options(max.print = 5000)
odf <- readRDS("uwlpuq.rds");idf <- readRDS("uwlpui.rds")

library(lavaan)

#bint, second model nearly good
mod <- 'bint =~ bint01 + bint02 + bint03 + bint04 + bint05 + bint06 + bint07 + bint08 + bint09 + bint10'

#good
mod <- 'bint =~ bint01 + bint02 + bint03 + bint04 + bint05 + bint06 + bint07 + bint08'# + bint09 + bint10
mod <- 'wlpri =~ wlpri1 + wlpri2 + wlpri3 + wlpri4'
mod <- 'regid =~ regid1 + regid2 + regid3 + regid4'
mod <- 'platt =~ place1 + place2 + place3 + place4'
mod <- 'natcn =~ natcn1 + natcn2 + natcn3 + natcn4'
mod <- 'penvb =~ penvb1 + penvb2 + penvb3 + penvb4 + penvb5 + penvb6'

#not good
mod <- 'envec =~ envec1 + envec2 + envec3 + envec4 + envec5 + envec6'
mod <- 'pecsa =~ pecsa1 + pecsa2 + pecsa3'
mod <- 'envpa =~ envpa1 + envpa2 + envpa3 + envpa4 + envpa5 + envpa6 + envpa7'

# Run the CFA
fit <- cfa(mod, data = idf,estimator = "DWLS",ordered = names(odf[, c(2, 8:13, 14:17, 18:24,25:30, 31:33, 93:96,186:195, 196:199, 200:203)]))
fitMeasures(fit, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))

#standardizedsolution(fit)[standardizedsolution(fit)$op=='=~',c(1:4,7)]
standardizedsolution(fit)[standardizedsolution(fit)$op=='=~',c(1:4,7)][order(-abs(standardizedsolution(fit)[standardizedsolution(fit)$op=='=~',c(1:4,7)][, 4])),]
