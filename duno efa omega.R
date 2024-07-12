# Load necessary library
library(psych)

# Select the variables for EFA
selected_vars <- c("wlim01", "wlim02", "wlim03", "wlim04", "wlim05", "wlim06", 
                   "wlim07", "wlim08", "wlim09", "wlim10", "wlim11", "wlim12", "wlim13")

# Subset the data to include only the selected variables
efa_data <- odf[, selected_vars]

# Run the EFA
efa_result <- psych::fa(efa_data, nfactors = 3, rotate = "varimax", fm = "ml")

# Print the factor loadings
print(efa_result$loadings, cutoff = 0.3, sort = TRUE)


# Load necessary library
library(psych)

# Select the variables for Omega analysis
selected_vars <- c("wlim01", "wlim02", "wlim03", "wlim04", "wlim05", "wlim06", 
                   "wlim07", "wlim08", "wlim09", "wlim10", "wlim11", "wlim12", "wlim13")
selected_vars <- c("wlbe01", "wlbe02", "wlbe03", "wlbe04", "wlbe05", "wlbe06", 
                   "wlbe07", "wlbe08", "wlbe09", "wlbe10", "wlbe11", "wlbe12")
selected_vars <- c("wlbe02", "wlbe03", "wlbe04", "wlbe05", "wlbe06", 
                   "wlbe07", "wlbe08", "wlbe10", "wlbe11", "wlbe12")
selected_vars <- c("wlim01", "wlim02", "wlim03", "wlim04", "wlim05", "wlim06", 
                   "wlim07", "wlim08", "wlim09", "wlim10", "wlim11", "wlim12", "wlim13",
                   "wlbe01", "wlbe02", "wlbe03", "wlbe04", "wlbe05", "wlbe06", 
                   "wlbe07", "wlbe08", "wlbe09", "wlbe10", "wlbe11", "wlbe12",
                   "wlac01", "wlac02", "wlac03", "wlac04", "wlac05", 
                   "wlac06", "wlac07", "wlac09")
#biocentric
selected_vars <- c("wlim01", "wlim02", "wlim03", "wlim04", "wlim05", "wlim10", "wlim12", "wlim13")
#anthropocentric
selected_vars <- c("wlim06", "wlim07", "wlim08", "wlim09", "wlim11")

# Subset the data to include only the selected variables
omega_data <- edf[, selected_vars]

# Run Omega analysis
omega_result <- psych::omega(omega_data)

# Print the Omega coefficients
print(omega_result)




install.packages("EGAnet")
library(EGAnet)

# Assuming `idf` is your dataframe and it contains the selected variables
selected_vars <- c("wlim01", "wlim02", "wlim03", "wlim04", "wlim05", "wlim06", 
                   "wlim07", "wlim08", "wlim09", "wlim10", "wlim11", "wlim12", "wlim13",
                   "wlbe01", "wlbe02", "wlbe03", "wlbe04", "wlbe05", "wlbe06", 
                   "wlbe07", "wlbe08", "wlbe09", "wlbe10", "wlbe11", "wlbe12",
                   "wlac01", "wlac02", "wlac03", "wlac04", "wlac05", 
                   "wlac06", "wlac07", "wlac08", "wlac09")

selected_vars <- c("wlim01", "wlim02", "wlim03", "wlim04", "wlim05", "wlim06", 
                   "wlim07", "wlim08", "wlim09", "wlim10", "wlim11", "wlim12", "wlim13")

selected_vars <- c("wlbe01", "wlbe02", "wlbe03", "wlbe04", "wlbe05", "wlbe06", 
                   "wlbe07", "wlbe08", "wlbe09", "wlbe10", "wlbe11", "wlbe12")
selected_vars <- c("wlprot", "bioccv", "antrcv", "proenb", "natcon", "placid")
# Subset the data to include only the selected variables
ega_data <- idf[, selected_vars]

# Run EGA
ega_results <- EGA(ega_data, method = "TMFG")
ega_results <- EGA(ndf, method = "TMFG")
ega_results <- EGA(ega_data, method = "glasso",algorithm = "louvain")
ega_results <- EGA(ega_data, method = "walktrap")
EGA(ega_data,)
?EGA
# Plot the EGA results
plot(ega_results)




# Load the lavaan package
library(lavaan)

# Define the model for biocentric variables
mod <- '
  bioc =~ wlim01 + wlim02 + wlim03 + wlim04 + wlim13
  anth =~ wlim06 + wlim07 + wlim08 + wlim09 + wlim11

'


# Fit the models
fit <- cfa(mod, data = idf,ordered=T)

# Summarize the results
summary(bioc_fit)

lavInspect(fit, "cov.lv")
fitMeasures(fit, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))

#factor loadings
standardizedsolution(fit)[standardizedsolution(fit)$op=='=~',c(1:4,7)]
#standardizedsolution(fit)[standardizedsolution(fit)$op=='=~',c(1:4,7)][order(-abs(standardizedsolution(fit)[standardizedsolution(fit)$op=='=~',c(1:4,7)][, 4])),]
lavaan::fitted.values(fit)
#correlations between lvs
standardizedsolution(fit)[standardizedsolution(fit)$op == '~~' & standardizedsolution(fit)$lhs != standardizedsolution(fit)$rhs, c(1:4, 7)]

predict(fit)[,1]
predict(fit)[,2]
hist(predict(fit)[,1])
hist(predict(fit)[,2])
