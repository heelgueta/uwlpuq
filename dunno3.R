library(lavaan)

# Define the CFA model
mod <- '
biocef =~ wlim01 + wlim02 + wlim03 + wlim04
humanf =~ wlim06 + wlim07 + wlim08 + wlim09
'

mod <-'
biocef =~ tpbe10 + mbbe05 + mbbe10 + tpbe05
indivf =~ tpbe08 + tpbe06 + mbbe06 + mbbe08'
socief =~ mbbe12 + tpbe12 + tpbe11 + mbbe11 


mod <- '
  # General factor
  G =~ tpbe10 + mbbe05 + mbbe10 + tpbe05 + mbbe12 + tpbe12 + tpbe11 + mbbe11 + tpbe08 + tpbe06 + mbbe06 + mbbe08
  
  # Specific factors
  biocef =~ tpbe10 + mbbe05 + mbbe10 + tpbe05
  socief =~ mbbe12 + tpbe12 + tpbe11 + mbbe11
  indivf =~ tpbe08 + tpbe06 + mbbe06 + mbbe08
  
  # Orthogonalize the specific factors
  G ~~ 0*biocef
  G ~~ 0*socief
  G ~~ 0*indivf
  biocef ~~ 0*socief
  biocef ~~ 0*indivf
  socief ~~ 0*indivf
'




# Run the CFA
fit <- cfa(mod, data = odf, estimator = "WLSMV",ordered=T)
lavInspect(fit, "cov.lv")

# Summarize the fit measures
fitMeasures(fit, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))

# Standardized factor loadings
standardizedsolution(fit)[standardizedsolution(fit)$op == "=~", c(1:4, 7)]
standardizedsolution(fit)[standardizedsolution(fit)$op=='=~',c(1:4,7)][order(-abs(standardizedsolution(fit)[standardizedsolution(fit)$op=='=~',c(1:4,7)][, 4])),]

