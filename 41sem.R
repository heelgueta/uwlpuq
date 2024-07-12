# Load the lavaan package
library(lavaan)

# Define the SEM model
mod <- '
  #wlprot Wetland protection (behavioral intentions + priority of wetland protection)
  #bioccv Biocentric valuation of wetlands
  #antrcv Anthropocentric valuation of wetlands
  #proenb Previous pro-environmental behavior
  #natcon Connectedness with nature
  #placid Place identification (regional identification + place attachment)
  
  # Measurement model
  bioccv =~ wlim01 + wlim02 + wlim03 + wlim04
  antrcv =~ wlim06 + wlim07 + wlim08 + wlim09
  methdi =~ 1*wlim01 + 1*wlim02 + 1*wlim03 + 1*wlim04 + 1*wlim06 + 1*wlim07 + 1*wlim08 + 1*wlim09

  wlprot =~ bint03 + bint04 + bint05 + bint07 + bint08 + bint02 + wlpri1 + wlpri2 + wlpri3 + wlpri4
  proenb =~ penvb1 + penvb2 + penvb3 + penvb4 + penvb5 + penvb6
  natcon =~ natcn1 + natcn2 + natcn3 + natcn4
  placid =~ regid1 + regid2 + regid3 + regid4# + place1 + place2 + place3 + place4
  methli =~ + 1*bint03 + 1*bint04 + 1*bint05 + 1*bint07 + 1*bint08 + 1*bint02 + 1*wlpri1 + 1*wlpri2 + 1*wlpri3 + 1*wlpri4 + 1*penvb1 + 1*penvb2 + 1*penvb3 + 1*penvb4 + 1*penvb5 + 1*penvb6 + 1*natcn1 + 1*natcn2 + 1*natcn3 + 1*natcn4 regid1 + 1*regid2 + 1*regid3 + 1*regid4  
'
  # Structural model
  #wlprot ~ proenb + bioccv + antrcv + natcon + placid
  #proenb ~ natcon + placid + bioccv + antrcv
  #bioccv ~ natcon + placid
  #antrcv ~ natcon + placid

# edad, ubicación, 
# 

#  wlprf =~ wlpri1 + wlpri2 + wlpri3 + wlpri4
  

# Fit the SEM model
fit <- sem(mod, data = idf,ordered=T)

# Summarize the SEM results
#summary(fit)
ndf <- data.frame(predict(fit))
colnames(ndf)
fitMeasures(fit, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))

#factor loadings
standardizedsolution(fit)[standardizedsolution(fit)$op=='=~',c(1:4,7)]
#standardizedsolution(fit)[standardizedsolution(fit)$op=='=~',c(1:4,7)][order(-abs(standardizedsolution(fit)[standardizedsolution(fit)$op=='=~',c(1:4,7)][, 4])),]
#lavaan::fitted.values(fit)
#correlations between lvs
standardizedsolution(fit)[standardizedsolution(fit)$op == '~~' & standardizedsolution(fit)$lhs != standardizedsolution(fit)$rhs, c(1:4, 7)]
standardizedsolution(fit)[standardizedsolution(fit)$op == '~' & standardizedsolution(fit)$lhs != standardizedsolution(fit)$rhs, c(1:4, 7)]

library(semPlot)
semPaths(fit)



semPaths(fit, what="std", whatLabels = "std",
         edge.label.cex = .8,nCharEdges=0,nCharNodes = 0,
         structural=F,optimizeLatRes=F,
         style="mx",residuals = T,#residScale = 4,
         layout="tree", rotation=2,
         curvePivot = T,exoVar = F,intercepts = F,thresholds = F,
         sizeLat = 9,sizeLat2 = 9,
         sizeMan = 4,sizeMan2 = 1.75,
         edge.width=4)#,edge.alpha = 0.9)
