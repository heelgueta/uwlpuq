#overall exploration of open text vars
odf <- readRDS("uwlpuq.rds")

#other: text responses (wetlands section)
unique(odf$wlknot);table(odf$wlknot)
unique(odf$wlanot);table(odf$wlanot)
unique(odf$wlplot);table(odf$wlplot)
unique(odf$wlthot);table(odf$wlthot)

#other: text responses (maria behety and tres puentes)
unique(odf$mbacot);table(odf$mbacot)
unique(odf$mbtrot);table(odf$mbtrot)
unique(odf$mbprot);table(odf$mbprot)
unique(odf$tpacot);table(odf$tpacot)
unique(odf$tptrot);table(odf$tptrot)
unique(odf$tpprot);table(odf$tpprot)
unique(odf$mbnvot);table(odf$mbnvot)
unique(odf$tpnvot);table(odf$tpnvot)




if (requireNamespace("dplyr", quietly = TRUE)) {
  library(dplyr)
  
  recode_responses <- function(x) {
    dplyr::case_when(
      grepl("conserva|biodiversidad|protec|preserva|cuida", tolower(x)) ~ "Conservation and Biodiversity",
      grepl("recrea|entreteni|esparci|distra", tolower(x)) ~ "Recreation and Leisure",
      grepl("observa|contempla|ver|mirar", tolower(x)) ~ "Nature Observation",
      grepl("salud mental|relaj|desconex|tranquil|espiritual", tolower(x)) ~ "Mental Health and Relaxation",
      grepl("famili|niños", tolower(x)) ~ "Family Activities",
      grepl("educa", tolower(x)) ~ "Environmental Education",
      grepl("deporte|camina|ejercicio", tolower(x)) ~ "Physical Health and Exercise",
      grepl("pulmón verde|pulmon verde|área verde|zona verde", tolower(x)) ~ "Urban Green Space",
      grepl("aire|oxígeno", tolower(x)) ~ "Air Quality",
      grepl("turismo", tolower(x)) ~ "Tourism",
      grepl("natural", tolower(x)) ~ "Connection with Nature",
      TRUE ~ "Other"
    )
  }
} else {
  recode_responses <- function(x) {
    result <- character(length(x))
    result[grepl("conserva|biodiversidad|protec|preserva|cuida", tolower(x))] <- "Conservation and Biodiversity"
    result[grepl("recrea|entreteni|esparci|distra", tolower(x))] <- "Recreation and Leisure"
    result[grepl("observa|contempla|ver|mirar", tolower(x))] <- "Nature Observation"
    result[grepl("salud mental|relaj|desconex|tranquil|espiritual", tolower(x))] <- "Mental Health and Relaxation"
    result[grepl("famili|niños", tolower(x))] <- "Family Activities"
    result[grepl("educa", tolower(x))] <- "Environmental Education"
    result[grepl("deporte|camina|ejercicio", tolower(x))] <- "Physical Health and Exercise"
    result[grepl("pulmón verde|pulmon verde|área verde|zona verde", tolower(x))] <- "Urban Green Space"
    result[grepl("aire|oxígeno", tolower(x))] <- "Air Quality"
    result[grepl("turismo", tolower(x))] <- "Tourism"
    result[grepl("natural", tolower(x))] <- "Connection with Nature"
    result[result == ""] <- "Other"
    return(result)
  }
}

# Apply the recoding function to both variables
odf$mbbemi_recoded <- recode_responses(odf$mbbemi)
odf$tpbemi_recoded <- recode_responses(odf$tpbemi)

# Print vertical frequency tables for the recoded variables
print_vertical <- function(x, name) {
  cat("\n", name, "\n", sep="")
  sorted_table <- sort(table(x), decreasing = TRUE)
  for (i in seq_along(sorted_table)) {
    cat(sprintf("%-30s %d\n", names(sorted_table)[i], sorted_table[i]))
  }
}

# Print vertical frequency tables for the recoded variables
print_vertical(odf$mbbemi_recoded, "mbbemi_recoded")
print_vertical(odf$tpbemi_recoded, "tpbemi_recoded")


print_other_category <- function(original, recoded, name) {
  cat("\n", name, " - Responses categorized as 'Other':\n", sep="")
  other_responses <- unique(original[recoded == "Other"])
  for (response in other_responses) {
    cat("- ", response, "\n", sep="")
  }
}

# Print the "Other" category contents for both variables
print_other_category(odf$mbbemi, odf$mbbemi_recoded, "mbbemi")
print_other_category(odf$tpbemi, odf$tpbemi_recoded, "tpbemi")
