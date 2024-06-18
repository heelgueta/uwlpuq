# Perform the original analysis
result <- psych::describe(odf[, c(34,35,36,37)])[, c(1, 3)];title <- "What wetlands in the region are you aware of?"
result <- psych::describe(odf[, c(42:54)])[, c(1, 3)];title <- "What are some important functions that wetlands have?"
result <- psych::describe(odf[, c(57:67)])[, c(1, 3)];title <- "Which animals are you aware of that inhabit wetlands?"
result <- psych::describe(odf[, c(70:74)])[, c(1, 3)];title <- "Which plants are you aware of that inhabit wetlands?"
result <- psych::describe(odf[, c(78:88)])[, c(1, 3)];title <- "What are some threats that wetlands face nowadays that you're aware of?"

# Convert the result into a data frame
df <- data.frame(variable = rownames(result), percentage = result[, "mean"])

# Read the variable descriptions from the CSV file
var_descriptions <- read.csv("variable_descriptions.csv", stringsAsFactors = FALSE)

# Merge the data frame with the variable descriptions
df <- merge(df, var_descriptions, by = "variable", all.x = TRUE)

# Create the horizontal barplot using ggplot2
library(ggplot2)
ggplot(df, aes(x = reorder(description, percentage), y = percentage)) +
  geom_bar(stat = "identity", fill = "black") +
  coord_flip() +
  labs(x = "Variable Description", y = "Percentage", title = title) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

