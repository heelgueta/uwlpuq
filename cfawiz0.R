cfawiz <- function(data, scale_spec, estimator = "DWLS", ordered = TRUE, ...) {
  library(lavaan)
  
  # Function to run CFA for a single specification
  run_single_cfa <- function(spec) {
    if (grepl("\\+", spec)) {
      # Custom model specification
      model <- paste0(sub("\\+.*", "", spec), " =~ ", sub(".*\\+", "", spec))
      factor_name <- sub("\\+.*", "", spec)
    } else {
      # Prefix-based specification
      scale_items <- grep(paste0("^", spec), names(data), value = TRUE)
      model <- paste0(spec, " =~ ", paste(scale_items, collapse = " + "))
      factor_name <- spec
    }
    
    cfa_fit <- cfa(model, data = data, estimator = estimator, ordered = ordered, ...)
    
    fit_indices <- fitMeasures(cfa_fit, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))
    
    loadings <- standardizedSolution(cfa_fit)
    loadings <- loadings[loadings$op == "=~", c("lhs", "rhs", "est.std", "pvalue")]
    
    list(fit_indices = fit_indices, loadings = loadings, factor_name = factor_name)
  }
  
  # Ensure scale_spec is a list
  if (!is.list(scale_spec)) {
    scale_spec <- as.list(scale_spec)
  }
  
  # Run CFA for each specification
  results <- lapply(scale_spec, run_single_cfa)
  
  # Combine fit indices into a single table
  fit_indices <- do.call(cbind, lapply(results, function(x) x$fit_indices))
  colnames(fit_indices) <- sapply(results, function(x) x$factor_name)
  
  # Combine loadings
  loadings <- do.call(rbind, lapply(results, function(x) x$loadings))
  
  # Print results
  cat("Fit Indices:\n")
  print(fit_indices)
  
  cat("\nLoadings:\n")
  print(loadings)
  
  # Return results invisibly
  invisible(list(fit_indices = fit_indices, loadings = loadings))
}

cfawiz(odf, "natcn")
cfawiz(odf, c("natcn", "regid+place"))
cfawiz(odf, "bint")

wat <- cfawiz(odf, "natcn")
wat$loadings
wat$fit_indices
