##################################################################
# 22unide-nom.R                                                  #
# 2.2 Univariate descriptive analyses for politomous nominal vars#
##################################################################

#check categorical politomous vars
unique(odf$wlrspn);table(odf$wlrspn)

#check categorical politomous vars (maria behety and tres puentes)
unique(odf$mbnvis);table(odf$mbnvis)
unique(odf$tpnvis);table(odf$tpnvis)
unique(odf$mbtran);table(odf$mbtran)
unique(odf$tptran);table(odf$tptran)
unique(odf$mbbemi);table(odf$mbbemi)
unique(odf$tpbemi);table(odf$tpbemi)

#other: text responses (demog section)
unique(odf$natiot);table(odf$natiot)
unique(odf$bplaot);table(odf$bplaot)
unique(odf$ethnot);table(odf$ethnot)
unique(odf$occupa);table(odf$occupa)

#check "encuestadores"
unique(odf$intnam);table(odf$intnam)


#or

library(dplyr)

recode_responses <- function(x) {
  case_when(
    grepl("conserva|biodiversidad|protec|preserva|cuida|flora|fauna", tolower(x)) ~ "Conservation and Biodiversity",
    grepl("recrea|entreteni|esparci|distra|famili|niño|social|turismo|acceso|ciudad", tolower(x)) ~ "Recreation, Leisure, and Community Use",
    grepl("observa|contempla|ver|mirar|natural|conex|belleza|estética", tolower(x)) ~ "Nature Appreciation and Aesthetics",
    grepl("salud|relaj|desconex|tranquil|espiritual|bienestar|deporte|camina|ejercicio", tolower(x)) ~ "Health and Wellbeing",
    grepl("educa|conocimiento|investiga|aprend", tolower(x)) ~ "Environmental Education and Research",
    grepl("pulmón verde|pulmon verde|área verde|zona verde|aire|oxígeno|clima|regula|inundaciones", tolower(x)) ~ "Urban Environmental Services",
    grepl("todos|todo|tres|múltiples", tolower(x)) ~ "Multiple Benefits",
    grepl("NA|ninguna|ninguno|no aplica", tolower(x)) ~ "No Response",
    TRUE ~ "Other"
  )
}

# Apply the recoding function to both variables
odf$mbbemi_recoded <- recode_responses(odf$mbbemi)
odf$tpbemi_recoded <- recode_responses(odf$tpbemi)






# Print vertical frequency tables for the recoded variables
print_vertical(odf$mbbemi_recoded, "mbbemi_recoded")
print_vertical(odf$tpbemi_recoded, "tpbemi_recoded")

# Function to print "Other" category contents
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























###########################################33



vtables <- function(df) {
  if (!require(stringr)) install.packages("stringr")
  library(stringr)
  
  truncate_string <- function(s, max_length = 30) {
    if (str_length(s) <= max_length) return(str_pad(s, max_length, "right"))
    start <- str_sub(s, 1, 14)
    end <- str_sub(s, -13)
    paste0(start, "...", end)
  }
  
  vardesc_df <- NULL
  if (file.exists("vardesc.csv")) {
    vardesc_df <- try(read.csv("vardesc.csv", stringsAsFactors = FALSE), silent = TRUE)
    if (inherits(vardesc_df, "try-error")) vardesc_df <- NULL
  }
  
  total_df_rows <- nrow(df)
  
  for (col in names(df)) {
    var_desc <- ""
    if (!is.null(vardesc_df)) {
      var_desc <- vardesc_df$vardesc[vardesc_df$varname == col]
      if (length(var_desc) == 0) var_desc <- ""
    }
    
    cat("\n", col, if(var_desc != "") paste(" -", var_desc) else "", "\n", sep="")
    
    freq_table <- table(df[[col]], useNA = "ifany")
    
    valid_total <- sum(freq_table[!is.na(names(freq_table)) & names(freq_table) != ""])
    
    cat(sprintf("%-30s %5s %6s %6s\n", "levels", "n", "%val", "%tot"))
    
    for (i in seq_along(freq_table)) {
      category_name <- names(freq_table)[i]
      if (is.na(category_name)) category_name <- "NA"
      if (category_name == "") category_name <- "<empty>"
      
      truncated_name <- truncate_string(category_name, 30)
      
      count <- freq_table[i]
      percentage_valid <- if (category_name %in% c("NA", "<empty>")) NA else (count / valid_total) * 100
      percentage_total <- (count / total_df_rows) * 100
      
      if (is.na(percentage_valid)) {
        cat(sprintf("%-30s %5d %6s %6.2f\n", truncated_name, count, "-", percentage_total))
      } else {
        cat(sprintf("%-30s %5d %6.2f %6.2f\n", truncated_name, count, percentage_valid, percentage_total))
      }
    }
  }
}

vtables(odf[, c(3, 5, 97, 110,  128, 139, 153, 171, 184, 204, 206, 208, 210, 211, 212, 213, 215, 216, 217)])
vtables(odf$tpbemi_recoded)



library(dplyr)
library(stringr)

recode_benefit <- function(x) {
  case_when(
    str_detect(tolower(x), "recrea|famil|entrete|esparci|paseo|jugar") ~ "Recreation and Family Time",
    str_detect(tolower(x), "conserva|biodivers|fauna|flora|proteg|cuidar|preserva") ~ "Conservation and Biodiversity",
    str_detect(tolower(x), "salud mental|bienestar|relaj|desconex|tranquil|espiritual") ~ "Mental Health and Wellbeing",
    str_detect(tolower(x), "natur|contempla|observa|ver|mirar|belleza|paisaje") ~ "Nature Appreciation",
    str_detect(tolower(x), "salud|deporte|caminar|aire|respir|oxígeno") ~ "Physical Health and Sports",
    str_detect(tolower(x), "educa|aprend|conocimiento|investiga") ~ "Education and Learning",
    str_detect(tolower(x), "más de (un|uno)|vari[ao]s") ~ "More than one benefit",
    is.na(x) ~ NA_character_,
    TRUE ~ "Other"
  )
}

odf <- odf %>%
  mutate(
    mbbemi_recoded = recode_benefit(mbbemi),
    tpbemi_recoded = recode_benefit(tpbemi)
  )

vtables(odf[, c("mbbemi_recoded", "tpbemi_recoded")])

vtables(odf[, c(3, 5, 97, 110,  226, 139, 153, 227, 184, 204, 206, 208, 211, 213, 215, 216, 217)])

library(writexl); library(readr); write_xlsx(do.call(rbind, lapply(names(odf[, c(3, 5, 97, 110, 226, 139, 153, 227, 184, 204, 206, 208, 211, 213, 215, 216, 217)]), function(var) {ft <- table(odf[[var]], useNA = "ifany"); vt <- sum(ft[!is.na(names(ft)) & names(ft) != ""]); vardesc_df <- read_csv("vardesc.csv", show_col_types = FALSE); vardesc <- vardesc_df$vardesc[vardesc_df$varname == var]; data.frame(Variable = c(paste0(var, " - ", vardesc), rep("", length(ft))), levels = c("levels", names(ft)), n = c("n", as.vector(ft)), val_percent = c("%val", ifelse(names(ft) %in% c(NA, ""), "-", sprintf("%.2f", ft/vt*100))), tot_percent = c("%tot", sprintf("%.2f", ft/nrow(odf)*100)))})), "vtables_output.xlsx")
vtables(odf[, c(216, 217)])

vtables(odf$agegrp)
mean(odf$agepar)
sd(odf$agepar)
hist(odf$agepar)
