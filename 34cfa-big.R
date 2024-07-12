#load data, setup
options(digits = 3);options(scipen = 999);options(max.print = 5000)
#load datasets if needed
odf <- readRDS("uwlpuq.rds")
idf <- readRDS("uwlpui.rds")

library(lavaan)

mod <- '
#  bintf =~ bint03 + bint04 + bint05 + bint07 + bint08 + bint02 #bint01 + bint06 + 
#  penvf =~ penvb1 + penvb2 + penvb3 + penvb4 + penvb5 + penvb6
#  wlprf =~ wlpri1 + wlpri2 + wlpri3 + wlpri4
#  natcf =~ natcn1 + natcn2 + natcn3 + natcn4
  RegiId =~ regid1 + regid2 + regid3 + regid4
  PlacAt =~ place1 + place2 + place3 + place4
#  biocf =~ wlim01 + wlim02 + wlim03 + wlim04 #+ wlim13
#  anthf =~ wlim06 + wlim07 + wlim08 + wlim09 #+ wlim11
'
mod <- '
#  bintf =~ bint03 + bint04 + bint05 + bint07 + bint08 + bint02 #bint01 + bint06 + 
#  penvf =~ penvb1 + penvb2 + penvb3 + penvb4 + penvb5 + penvb6
#  wlprf =~ wlpri1 + wlpri2 + wlpri3 + wlpri4
#  natcf =~ natcn1 + natcn2 + natcn3 + natcn4
  PlacId =~ regid1 + regid2 + regid3 + regid4 +  place1 + place2 + place3 + place4
  
#  biocf =~ wlim01 + wlim02 + wlim03 + wlim04 #+ wlim13
#  anthf =~ wlim06 + wlim07 + wlim08 + wlim09 #+ wlim11
'


mod <- '
  BehaInt =~ bint03 + bint04 + bint05 + bint07 + bint08 + bint02 #bint01 + bint06 + 
  PrevBeh =~ penvb1 + penvb2 + penvb3 + penvb4 + penvb5 + penvb6
#  wlprf =~ wlpri1 + wlpri2 + wlpri3 + wlpri4
  ConnNat =~ natcn1 + natcn2 + natcn3 + natcn4
  PlaceId =~ regid1 + regid2 + regid3 + regid4 + place1 + place2 + place3 + place4
#  placf =~ place1 + place2 + place3 + place4
  ImpoWL =~ wlim01 + wlim02 + wlim03 + wlim04 + wlim06 + wlim07 + wlim08 + wlim09
#BehaInt ~ PrevBeh + ConnNat + PlaceId + ImpoWL
'

#  mbtpf =~ mbfreq + mbleng + tpfreq + tpleng;tpfreq ~~ tpleng;mbfreq ~~ mbleng



# Run the CFA

fit <- cfa(mod, data = idf,ordered =TRUE)
fit <- sem(mod, data = idf,ordered =TRUE)

#lavInspect(fit, "cov.lv")
fitMeasures(fit, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))

#factor loadings
standardizedsolution(fit)[standardizedsolution(fit)$op=='=~',c(1:4,7)]
#standardizedsolution(fit)[standardizedsolution(fit)$op=='=~',c(1:4,7)][order(-abs(standardizedsolution(fit)[standardizedsolution(fit)$op=='=~',c(1:4,7)][, 4])),]
#lavaan::fitted.values(fit)
#correlations between lvs
standardizedsolution(fit)[standardizedsolution(fit)$op == '~~' & standardizedsolution(fit)$lhs != standardizedsolution(fit)$rhs, c(1:4, 7)]
standardizedsolution(fit)[standardizedsolution(fit)$op == '~' & standardizedsolution(fit)$lhs != standardizedsolution(fit)$rhs, c(1:4, 7)]

library(semPlot)


semPaths(fit, what="std", whatLabels = "std",
         edge.label.cex = .8,nCharEdges=0,nCharNodes = 0,
         structural=F,optimizeLatRes=F,
         #style="mx",residuals = T,#residScale = 4,
         layout="tree", rotation=1,
         curvePivot = T,exoVar = F,intercepts = F,thresholds = F,
         sizeLat = 7,sizeLat2 = 7,
         sizeMan = 4,sizeMan2 = 1.75,
         edge.width=4)



semPaths(fit, what="std", whatLabels = "std",
         edge.label.cex = .8,nCharEdges=0,nCharNodes = 0,
         structural=F,optimizeLatRes=F,
         #style="mx",residuals = T,#residScale = 4,
         layout="spring", rotation=1,
         curvePivot = T,exoVar = F,intercepts = F,thresholds = F,
         sizeLat = 7,sizeLat2 = 7,
         sizeMan = 4,sizeMan2 = 1.75,
         edge.width=4)


semPaths(fit, what="std", whatLabels = "std",
         edge.label.cex = .8,nCharEdges=0,nCharNodes = 0,
         structural=F,optimizeLatRes=F,
         style="mx",residuals = T,#residScale = 4,
         layout="tree", rotation=2,
         curvePivot = T,exoVar = F,intercepts = F,thresholds = F,
         sizeLat = 7,sizeLat2 = 7,
         sizeMan = 4,sizeMan2 = 1.75,
         edge.width=4,edge.alpha = 0.9)

semPaths(fit, what="std", whatLabels = "std",
         edge.label.cex = .8,nCharEdges=0,nCharNodes = 0,
         structural=F,optimizeLatRes=F,
         style="mx",residuals = T,#residScale = 4,
         layout="tree", rotation=2,
         curvePivot = T,exoVar = F,intercepts = F,thresholds = F,
         sizeLat = 7,sizeLat2 = 7,
         sizeMan = 4,sizeMan2 = 1.75,
         edge.width=4,edge.alpha = 0.9)



semPaths(fit, what="std", whatLabels = "std",
         edge.label.cex = .8,nCharEdges=0,nCharNodes = 0,
         structural=F,optimizeLatRes=F,
         #style="mx",residuals = T,#residScale = 4,
         layout="tree", rotation=1,
         curvePivot = T,exoVar = F,intercepts = F,thresholds = F,
         sizeLat = 7,sizeLat2 = 7,
         sizeMan = 4,sizeMan2 = 1.75,
         edge.width=1)


#add factor scores to idf
idf <- cbind(idf, lavPredict(fit))
hist(idf$bintf)
hist(idf$penvf)
plot(idf$penvf,idf$bintf)

plot(idf$bintf,idf$penvf)
plot(idf$penvf,idf$natcf)


idf$anthf
library(ggplot2)
library(dplyr)
library(tidyr)

# Create a named vector for custom subtitles
custom_titles <- c(
  bintf = "Wetland protection behavioral intentions",
  penvf = "Previous pro-environmental behaviors",
  wlprf = "Wetland protection attitudes",
  natcf = "Conectedness with nature",
  regif = "Regional identification",
  placf = "Place attachment",
  mbtpf = "Visiting the urban wetlands",
  biocf = "Biocentric valuation of wetlands",
  anthf = "Anthropocentric valuation of wetlands"
)
rm(idf_long)

idf_long <- idf %>%
  pivot_longer(cols = starts_with("bintf") | 
                 starts_with("penvf") |
                 starts_with("anthf") |
                 starts_with("natcf") |
                 starts_with("regif") |
                 starts_with("placf") |
                 starts_with("biocf") |
                 starts_with("humcf") |
                 starts_with("mbtpf"),
               names_to = "factor",
               values_to = "score")


# Assuming idf_long is already created
# Plot histograms using ggplot2
ggplot(idf_long, aes(x = score)) +
  geom_histogram(bins = 50, fill = "black", color = "white", alpha = 0.9) +
  facet_wrap(~ factor, scales = "free_x", labeller = labeller(factor = custom_titles), strip.position = "top") +
  labs(title = "Histograms of Factor Scores",
       x = "Factor Score",
       y = "Frequency") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10), 
    strip.placement = "outside",  # Move the strip to the top
    plot.title = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 10, face = "bold"),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey80")
  ) +
  scale_x_continuous(limits = c(-2.5, 2.5))

