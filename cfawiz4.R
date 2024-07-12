cfawiz <- function(data, scale_spec, estimator = "DWLS", ordered = TRUE, 
                   colored = "theme2", pvals = "asterisks", fitm = "short", 
                   remove = NULL, vardesc = "vardesc.csv", ...) {
  # Welcome message
  cat(crayon::cyan$bold("Welcome to cfawiz, part of the psywiz package!\n"))
  
  # Load required libraries
  library(lavaan)
  library(crayon)
  
  # Helper functions
  format_pvalue <- function(p) {
    if (pvals == "asterisks") {
      ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))
    } else if (pvals == "value") {
      ifelse(p < 0.001, "<.001", sprintf("%.3f", p))
    } else {
      ""
    }
  }
  
  color_gradient <- function(x) {
    if (colored == "none") return(sprintf("%6.3f", x))
    
    abs_x <- abs(x)
    colors <- if (colored == "theme1") {
      c("#FF0000", "#FF4000", "#FF8000", "#FFBF00", "#FFFF00", "#BFFF00", "#80FF00", "#40FF00", "#00FF00", "#00FF40")
    } else {
      c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061")
    }
    
    color_index <- findInterval(abs_x, seq(0, 1, length.out = length(colors) + 1))
    make_style(colors[color_index])(sprintf("%6.3f", x))
  }
  
  # Load variable descriptions
  var_descriptions <- NULL
  if (!is.null(vardesc) && vardesc != "none") {
    tryCatch({
      var_descriptions <- read.csv(vardesc, stringsAsFactors = FALSE)
      rownames(var_descriptions) <- var_descriptions$varname
    }, error = function(e) {
      cat("Warning: Could not load variable descriptions from", vardesc, "\n")
    })
  }
  
  # Ensure scale_spec is a list
  if (!is.list(scale_spec)) scale_spec <- as.list(scale_spec)
  
  # Function to run CFA for a single specification
  run_single_cfa <- function(spec) {
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
    
    # Remove specified items
    if (!is.null(remove)) {
      scale_items <- setdiff(scale_items, remove)
    }
    
    model <- paste0(factor_name, " =~ ", paste(scale_items, collapse = " + "))
    
    tryCatch({
      cfa_fit <- cfa(model, data = data, estimator = estimator, ordered = ordered, ...)
      fit_indices <- fitMeasures(cfa_fit)
      loadings <- standardizedSolution(cfa_fit)
      loadings <- loadings[loadings$op == "=~", c("lhs", "rhs", "est.std", "pvalue")]
      list(fit_indices = fit_indices, loadings = loadings, factor_name = factor_name, cfa_fit = cfa_fit)
    }, error = function(e) {
      cat("Error in CFA for", factor_name, ":", conditionMessage(e), "\n")
      NULL
    })
  }
  
  # Run CFA for each specification
  results <- lapply(scale_spec, run_single_cfa)
  results <- results[!sapply(results, is.null)]
  
  if (length(results) == 0) {
    cat("No valid CFA models could be run. Please check your data and specifications.\n")
    return(invisible(NULL))
  }
  
  # Combine fit indices into a single table
  fit_indices <- do.call(cbind, lapply(results, function(x) x$fit_indices))
  colnames(fit_indices) <- sapply(results, function(x) x$factor_name)
  
  # Print results
  cat("\nFit Indices:\n")
  fit_measures <- switch(fitm,
                         "short" = c("chisq", "df", "pvalue", "cfi", "rmsea", "srmr"),
                         "mid" = c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"),
                         "long" = c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "aic", "bic"),
                         "full" = rownames(fit_indices)
  )
  print(round(fit_indices[fit_measures, , drop = FALSE], 3), print.gap = 3)
  
  cat("\nLoadings:\n")
  for (i in seq_along(results)) {
    if (i > 1) cat(paste(rep("-", 30), collapse = ""), "\n")
    cat(results[[i]]$factor_name, ":\n")
    loadings <- results[[i]]$loadings
    loadings$est.std <- round(loadings$est.std, 3)
    loadings$pvalue <- format_pvalue(loadings$pvalue)
    loadings <- loadings[order(abs(loadings$est.std), decreasing = TRUE), ]
    rownames(loadings) <- loadings$rhs
    
    loadings$est.std_colored <- sapply(loadings$est.std, color_gradient)
    
    console_width <- options()$width
    
    for (j in 1:nrow(loadings)) {
      loading_text <- sprintf("%-10s %s %s", 
                              rownames(loadings)[j], 
                              loadings$est.std_colored[j], 
                              loadings$pvalue[j])
      
      if (!is.null(var_descriptions) && rownames(loadings)[j] %in% rownames(var_descriptions)) {
        desc <- var_descriptions[rownames(loadings)[j], "vardesc"]
        remaining_width <- console_width - nchar(loading_text) - 3
        if (nchar(desc) > remaining_width) {
          desc <- substr(desc, 1, remaining_width - 3)
          desc <- paste0(desc, "...")
        }
        loading_text <- paste(loading_text, "#", desc)
      }
      
      cat(loading_text, "\n")
    }
    cat("\n")
  }
  
  # Prepare return object
  return_object <- list(
    fit_indices = fit_indices,
    std_loadings = do.call(rbind, lapply(results, function(x) x$loadings)),
    unstd_loadings = do.call(rbind, lapply(results, function(x) parameterEstimates(x$cfa_fit)[parameterEstimates(x$cfa_fit)$op == "=~", ])),
    all_fit_measures = fit_indices,
    lavaan_models = lapply(results, function(x) x$cfa_fit)
  )
  
  invisible(return_object)
}



cfawiz(idf,c("bint","regid","place","envec"),pvals = "value")
