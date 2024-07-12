cfawiz <- function(data, scale_prefix, estimator = "DWLS", ordered = TRUE, ...) {
  # 1. Identify relevant columns
  scale_items <- grep(paste0("^", scale_prefix), names(data), value = TRUE)
  
  # 2. Create the lavaan model string
  model <- paste0(scale_prefix, " =~ ", paste(scale_items, collapse = " + "))
  
  # 3. Run the CFA
  cfa_fit <- lavaan::cfa(model, 
                         data = data, 
                         estimator = estimator, 
                         ordered = ordered,
                         ...)
  
  # 4. Extract and format results
  fit_indices <- lavaan::fitMeasures(cfa_fit, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))
  
  # Format fit indices
  fit_summary <- sprintf(
    "%-15s %8.3f\n%-15s %8d\n%-15s %8.3f\n%-15s %8.3f\n%-15s %8.3f\n%-15s %8.3f\n%-15s %8.3f\n%-15s %8.3f\n%-15s %8.3f",
    "Chi-square", fit_indices["chisq"],
    "df", as.integer(fit_indices["df"]),
    "p-value", fit_indices["pvalue"],
    "CFI", fit_indices["cfi"],
    "TLI", fit_indices["tli"],
    "SRMR", fit_indices["srmr"],
    "RMSEA", fit_indices["rmsea"],
    "RMSEA-LO", fit_indices["rmsea.ci.lower"],
    "RMSEA-UP", fit_indices["rmsea.ci.upper"]
  )
  
  # Get both standardized and unstandardized solutions
  loadings_std <- lavaan::standardizedSolution(cfa_fit)
  loadings_unstd <- lavaan::parameterEstimates(cfa_fit)
  
  # Merge standardized and unstandardized loadings
  loadings <- merge(
    loadings_std[loadings_std$op == "=~", c("lhs", "rhs", "est.std", "pvalue")],
    loadings_unstd[loadings_unstd$op == "=~", c("lhs", "rhs", "est", "se")],
    by = c("lhs", "rhs")
  )
  
  # Rename columns for clarity
  names(loadings) <- c("factor", "item", "std", "p.value", "unstd", "std.error")
  
  # 5. Create a summary list
  summary_list <- list(
    fit_summary = fit_summary,
    fit_indices = fit_indices,
    loadings = loadings
  )
  
  # 6. Print summary and return results
  cat("Fit Summary:\n")
  cat(fit_summary, "\n\n")
  cat("Loadings:\n")
  print(loadings)
  
  invisible(summary_list)
}



cfawiz(odf,"natcn")
cfawiz(odf,"regid")
