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

# Subset the data to include only the selected variables
omega_data <- odf[, selected_vars]

# Run Omega analysis
omega_result <- psych::omega(omega_data)

# Print the Omega coefficients
print(omega_result)
