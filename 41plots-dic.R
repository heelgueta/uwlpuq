###########################################################################
# 41plots-dic.R                                                           #
# 4.1 Plots/megaplots Univariate descriptive analyses for dichotomous vars#
###########################################################################

odf <- readRDS("uwlpuq.rds")

# Install and load the patchwork package
# install.packages("patchwork")
library(patchwork)

# Read the variable descriptions from the CSV file
var_descriptions <- read.csv("variable_descriptions.csv", stringsAsFactors = FALSE)

# Define a function to create a plot for each result
create_plot <- function(result, title) {
  # Convert the result into a data frame
  df <- data.frame(variable = rownames(result), percentage = result[, "mean"])
  
  # Merge the data frame with the variable descriptions
  df <- merge(df, var_descriptions, by = "variable", all.x = TRUE)
  
  # Create the horizontal barplot using ggplot2
  plot <- ggplot(df, aes(x = reorder(description, percentage), y = percentage)) +
    geom_bar(stat = "identity", fill = "black") +
    coord_flip() +
    labs(x = "Spontaneous responses", y = "Percentage", title = title) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 12),
          axis.text.y = element_text(size = 8),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10)),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())
  
  return(plot)
}

# Create separate plots for each result
plot1 <- create_plot(psych::describe(odf[, c(34,35,36,37,38)])[, c(1, 3)], "What wetlands in the region are you aware of?")
plot2 <- create_plot(psych::describe(odf[, c(42:54)])[, c(1, 3)], "What are some important functions that wetlands have?")
plot3 <- create_plot(psych::describe(odf[, c(57:67)])[, c(1, 3)], "Which animals are you aware of that inhabit wetlands?")
plot4 <- create_plot(psych::describe(odf[, c(70:74,75)])[, c(1, 3)], "Which plants are you aware of that inhabit wetlands?")
plot5 <- create_plot(psych::describe(odf[, c(78:88,91)])[, c(1, 3)], "What are some threats that wetlands face?")

# Combine the plots into a mega plot
mega_plot <- plot1 / plot2 / plot3 / plot4 / plot5 &
  theme(plot.margin = margin(20, 20, 20, 20))

# Display the mega plot
print(mega_plot)