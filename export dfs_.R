# Assuming vardesc is already loaded, if not, load it with:
# vardesc <- read.csv("vardesc.csv", stringsAsFactors = FALSE)

# Rename the 'varname' column to 'variable' to match our merge operation
names(vardesc)[names(vardesc) == "varname"] <- "variable"

# Create a data frame of variable names from idf
idf_names <- data.frame(variable = names(idf))

# Merge descriptions with variable names
desc_merged <- merge(idf_names, vardesc, by = "variable", all.x = TRUE)

# Set variable labels for the original dataset
for (var in names(idf)) {
  if (var %in% desc_merged$variable) {
    label <- desc_merged$vardesc[desc_merged$variable == var]
    attr(idf[[var]], "label") <- label
  }
}

# Check labels
str(idf)


# Save the labeled dataset as an RDS file
saveRDS(idf, file = "idf.rds")








# Install necessary packages if you haven't already
install.packages(c("haven", "jmvcore"))

# Load the required libraries
library(haven)
library(jmvcore)

# Assuming 'idf' is your labeled dataset

# 1. Saving as .omv (Jamovi) file
# Create a data frame with variable names and labels
var_labels <- data.frame(
  name = names(idf),
  label = sapply(idf, function(x) attr(x, "label"))
)

# Create a Jamovi dataset
jmv_data <- create_jamovi(idf)

# Add variable labels
jmv_data <- add_variable_labels(jmv_data, var_labels)

# Save as .omv file
saveRDS(jmv_data, file = "idf.omv", compress = FALSE)

# 2. Saving as .sav (SPSS) file
# First, ensure all character variables are converted to factors
idf[] <- lapply(idf, function(x) if(is.character(x)) as.factor(x) else x)

# Save as .sav file
write_sav(idf, "idf.sav")
