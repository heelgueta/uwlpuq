cfawiz <- function(data, scale_spec, estimator = "DWLS", ordered = TRUE, 
                   colored = "none", pvals = "asterisks", est = "std", fitm = "short", ...) {
  library(lavaan)
  if(colored != "none") {
    if(!requireNamespace("crayon", quietly = TRUE)) {
      install.packages("crayon")
    }
    library(crayon)
  }
  
  # 2. Helper functions
  # 2.1 Function to run CFA for a single specification
  run_single_cfa <- function(spec) {
    # 2.1.1 Parse the specification
    if (grepl("&", spec)) {
      prefixes <- strsplit(spec, "&")[[1]]
      factor_name <- paste(prefixes, collapse="_")
      scale_items <- unlist(lapply(prefixes, function(prefix) grep(paste0("^", prefix), names(data), value = TRUE)))
    } else if (grepl("\\+", spec)) {
      parts <- strsplit(spec, "\\+")[[1]]
      prefix <- trimws(parts[1])
      factor_name <- prefix
      scale_items <- c(grep(paste0("^", prefix), names(data), value = TRUE), trimws(parts[-1]))
    } else {
      factor_name <- spec
      scale_items <- grep(paste0("^", spec), names(data), value = TRUE)
    }
    
    original_items <- scale_items
    removed_items <- c()
    
    # 2.1.2 Attempt CFA, removing problematic items if necessary
    while(length(scale_items) > 2) {
      model <- paste0(factor_name, " =~ ", paste(scale_items, collapse = " + "))
      
      tryCatch({
        cfa_fit <- cfa(model, data = data, estimator = estimator, ordered = ordered, ...)
        
        fit_indices <- fitMeasures(cfa_fit, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))
        
        loadings <- standardizedSolution(cfa_fit)
        loadings <- loadings[loadings$op == "=~", c("lhs", "rhs", "est.std", "pvalue")]
        
        return(list(fit_indices = fit_indices, loadings = loadings, factor_name = factor_name, removed_items = removed_items))
      }, error = function(e) {
        cat("Error occurred. Removing problematic item and retrying...\n")
        problematic_item <- gsub(".*variable `(.*)' are empty.*", "\\1", e$message)
        scale_items <<- scale_items[scale_items != problematic_item]
        removed_items <<- c(removed_items, problematic_item)
      })
    }
    
    # 2.1.3 If CFA fails completely, return helpful message
    cat("OOPS! Something's not quite right. Unable to run CFA for", factor_name, "\n")
    cat("Suggestions:\n")
    cat("- Check for missing data or zero variance in your variables\n")
    cat("- Ensure you have at least 3 valid indicators for your factor\n")
    cat("- Verify that your data meets the assumptions for CFA\n")
    return(NULL)
  }
  
  # Updated color gradient function
  color_gradient <- function(x) {
    if (colored == "none") return(sprintf("%6.3f", x))
    
    abs_x <- abs(x)
    if (colored == "theme1") {
      colors <- c("#FF0000", "#FF4000", "#FF8000", "#FFBF00", "#FFFF00", 
                  "#BFFF00", "#80FF00", "#40FF00", "#00FF00", "#00FF40")
    } else if (colored == "theme2") {
      colors <- c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", 
                  "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061")
    } else {
      stop("Invalid color theme")
    }
    
    color_index <- findInterval(abs_x, seq(0, 1, length.out = length(colors) + 1))
    crayon::make_style(colors[color_index])(sprintf("%6.3f", x))
  }
  
  # Function to format p-values
  format_pvalue <- function(p) {
    if (pvals == "asterisks") {
      ifelse(p < 0.001, "***",
             ifelse(p < 0.01, "**",
                    ifelse(p < 0.05, "*", "")))
    } else if (pvals == "value") {
      ifelse(p < 0.001, "<.001", sprintf("%.3f", p))
    } else {
      ""
    }
  }
  
  
  # 3. Main function logic
  # 3.1 Ensure scale_spec is a list
  if (!is.list(scale_spec)) {
    scale_spec <- as.list(scale_spec)
  }
  
  # 3.2 Run CFA for each specification
  results <- lapply(scale_spec, run_single_cfa)
  
  # 3.3 Remove NULL results (failed CFAs)
  results <- results[!sapply(results, is.null)]
  
  if (length(results) == 0) {
    cat("No valid CFA models could be run. Please check your data and specifications.\n")
    return(invisible(NULL))
  }
  
  # 3.4 Combine fit indices into a single table
  fit_indices <- do.call(cbind, lapply(results, function(x) x$fit_indices))
  colnames(fit_indices) <- sapply(results, function(x) x$factor_name)
  
  # 4. Print results
  # 4.1 Print fit indices
  cat("Fit Indices:\n")
  fit_measures <- switch(fitm,
                         "short" = c("chisq", "df", "pvalue", "cfi", "rmsea", "srmr"),
                         "mid" = c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"),
                         "long" = c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "aic", "bic"),
                         "full" = names(fit_indices)
  )
  print(round(fit_indices[fit_measures, ], 3))
  
  # 4.2 Print loadings
  cat("\nLoadings:\n")
  for (i in seq_along(results)) {
    if (i > 1) cat(paste(rep("-", 30), collapse = ""), "\n")
    cat(results[[i]]$factor_name, ":\n")
    loadings <- results[[i]]$loadings
    loadings$est.std <- round(loadings$est.std, 3)
    loadings$est.unstd <- round(loadings$est, 3)
    loadings$pvalue <- format_pvalue(loadings$pvalue)
    loadings <- loadings[order(abs(loadings$est.std), decreasing = TRUE), ]
    rownames(loadings) <- loadings$rhs
    
    # Apply color gradient if requested
    loadings$est.std_colored <- sapply(loadings$est.std, color_gradient)
    
    for (j in 1:nrow(loadings)) {
      if (est == "std" || est == "both") {
        cat(sprintf("%-10s %s %s\n", 
                    rownames(loadings)[j], 
                    loadings$est.std_colored[j], 
                    loadings$pvalue[j]))
      }
      if (est == "unstd" || est == "both") {
        cat(sprintf("%-10s %6.3f %s\n", 
                    rownames(loadings)[j], 
                    loadings$est.unstd[j], 
                    loadings$pvalue[j]))
      }
    }
    cat("\n")
    
    # 4.3 Print removed items
    if (length(results[[i]]$removed_items) > 0) {
      cat("Removed items:", paste(results[[i]]$removed_items, collapse = ", "), "\n\n")
    }
  }
  
  # 5. Return results invisibly
  invisible(list(fit_indices = fit_indices, loadings = do.call(rbind, lapply(results, function(x) x$loadings))))
}


cfawiz(odf, c("bint","regid"),colored = "theme2", est="std")
