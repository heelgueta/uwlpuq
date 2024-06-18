#load data, setup
options(digits = 3);options(scipen = 999);options(max.print = 5000)
#load datasets if needed
odf <- readRDS("uwlpuq.rds")
idf <- readRDS("uwlpui.rds")

library(lavaan)

# Define the extended CFA model
mod <- '
  binte =~ bint01 + bint02 + bint03 + bint04 + bint05 + bint06 + bint07 + bint08
  wlpri =~ wlpri1 + wlpri2 + wlpri3 + wlpri4
  regid =~ regid1 + regid2 + regid3 + regid4
  platt =~ place1 + place2 + place3 + place4
  natcn =~ natcn1 + natcn2 + natcn3 + natcn4
  penvb =~ penvb1 + penvb2 + penvb3 + penvb4 + penvb5 + penvb6
'


# Run the CFA
fit <- cfa(mod, data = idf,estimator = "DWLS",ordered = names(odf[, c(2, 8:13, 14:17, 18:24,25:30, 31:33, 93:96,186:195, 196:199, 200:203)]))
fitMeasures(fit, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))

standardizedsolution(fit)[standardizedsolution(fit)$op=='=~',c(1:4,7)]
#standardizedsolution(fit)[standardizedsolution(fit)$op=='=~',c(1:4,7)][order(-abs(standardizedsolution(fit)[standardizedsolution(fit)$op=='=~',c(1:4,7)][, 4])),]

standardizedsolution(fit)[standardizedsolution(fit)$op=='~~',c(1:4,7)]
