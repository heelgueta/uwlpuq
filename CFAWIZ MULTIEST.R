cfawiz <- function(data, scale_spec, ordered = TRUE, 
                            colored = TRUE, pvals = "none", fitindices = "short", 
                            loadings = "std", remove = NULL, vardesc = NULL, ...) {
  # 1. Welcome message
  cat(crayon::cyan$bold("Welcome to cfawiz_multiest, part of the psywiz package!\n"))
  
  # 2. Load required libraries
  library(lavaan)
  library(crayon)
  
  # 3. Define estimators
  estimators <- c("ML", "ULS", "WLS", "DWLS", "MLR", "ULSM", "ULSMV")
  
  # 4. Helper function for p-value formatting
  format_pvalue <- function(p) {
    if (pvals == "asterisks") {
      ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))
    } else if (pvals == "value") {
      ifelse(p < 0.001, "<.001", sprintf("%.3f", p))
    } else {
      ""
    }
  }
  
  # 5. Helper function for color gradient
  color_gradient <- function(x) {
    if (!colored || is.na(x)) return(sprintf("%10s", ifelse(is.na(x), "NA", sprintf("%.3f", x))))
    
    abs_x <- abs(x)
    colors <- c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061")
    
    color_index <- findInterval(abs_x, seq(0, 1, length.out = length(colors) + 1))
    make_style(colors[color_index])(sprintf("%10.3f", x))
  }
  
  # Helper function to truncate estimator names
  truncate_name <- function(name, max_length = 8) {
    if (nchar(name) > max_length) {
      return(paste0(substr(name, 1, max_length - 2), ".."))
    }
    return(name)
  }
  
  # Helper function for color coding fit indices with more granularity
  color_code_fit <- function(measure, value) {
    if (!colored || is.na(value)) return(sprintf("%10s", ifelse(is.na(value), "NA", sprintf("%.3f", value))))
    
    value <- as.numeric(value)
    colors <- c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061")
    
    index <- switch(measure,
                    cfi = ,
                    tli = ,
                    nnfi = ,
                    rfi = ,
                    nfi = ,
                    ifi = ,
                    rni = ,
                    gfi = ,
                    agfi = ,
                    mfi = findInterval(value, c(-Inf, 0.70, 0.75, 0.80, 0.85, 0.90, 0.93, 0.95, 0.97, 0.99, Inf)),
                    rmsea = ,
                    rmsealo = ,
                    rmseaup = 11 - findInterval(value, c(-Inf, 0.01, 0.05, 0.08, 0.10, 0.12, 0.15, 0.20, 0.25, 0.30, Inf)),
                    srmr = ,
                    srmrben = ,
                    srmrbnm = 11 - findInterval(value, c(-Inf, 0.03, 0.05, 0.08, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, Inf)),
                    ave = findInterval(value, c(-Inf, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, Inf)),
                    cr = findInterval(value, c(-Inf, 0.50, 0.55, 0.60, 0.65, 0.70, 0.80, 0.85, 0.90, 0.95, Inf)),
                    0
    )
    
    # Ensure index is within the valid range
    index <- max(1, min(index, length(colors)))
    
    make_style(colors[index])(sprintf("%10.3f", value))
  }
  
  # 6. Load variable descriptions if provided
  var_descriptions <- NULL
  if (!is.null(vardesc)) {
    tryCatch({
      var_descriptions <- read.csv(vardesc, stringsAsFactors = FALSE)
      if(nrow(var_descriptions) == 0) {
        cat("Warning: vardesc file is empty.\n")
      } else {
        rownames(var_descriptions) <- var_descriptions$varname
        cat("Successfully loaded", nrow(var_descriptions), "variable descriptions.\n")
      }
    }, error = function(e) {
      cat("Error loading variable descriptions from", vardesc, ":\n")
      cat(conditionMessage(e), "\n")
    })
  }
  
  # 7. Ensure scale_spec is a list
  if (!is.list(scale_spec)) scale_spec <- list(scale_spec)
  
  # 8. Function to run CFA for a single specification with multiple estimators
  run_single_cfa_multi <- function(spec) {
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
    
    if (!is.null(remove)) {
      scale_items <- setdiff(scale_items, remove)
    }
    
    model <- paste0(factor_name, " =~ ", paste(scale_items, collapse = " + "))
    
    # Display model and ntotal
    cat("\nModel being tested:\n")
    cat(model, "\n")
    cat("Dataframe:", deparse(substitute(data)), "\n")
    cat("Number of observations:", nrow(data), "\n\n")
    
    results <- list()
    for (est in estimators) {
      tryCatch({
        use_ordered <- ordered && !(est %in% c("ML", "MLR"))
        cfa_fit <- cfa(model, data = data, estimator = est, ordered = use_ordered, ...)
        fit_indices <- fitMeasures(cfa_fit)
        loadings <- standardizedSolution(cfa_fit)
        loadings <- loadings[loadings$op == "=~", c("lhs", "rhs", "est.std", "pvalue")]
        results[[est]] <- list(fit_indices = fit_indices, loadings = loadings, cfa_fit = cfa_fit)
      }, error = function(e) {
        cat("Note: CFA for", factor_name, "with estimator", est, "failed.\n")
        cat("Reason:", conditionMessage(e), "\n\n")
      })
    }
    
    list(results = results, factor_name = factor_name, scale_items = scale_items)
  }
  
  # 9. Run CFA for each specification
  all_results <- lapply(scale_spec, run_single_cfa_multi)
  
  # 10. Process and print results for each specification
  for (spec_result in all_results) {
    cat("\nResults for", spec_result$factor_name, ":\n")
    
    if (length(spec_result$results) == 0) {
      cat("No valid results obtained for this specification.\n")
      next
    }
    
    # 11. Print fit indices
    cat("\nFit Indices:\n")
    fit_measures <- c(
      "chisq", "df", "pvalue", 
      "cfi", "tli", "nnfi", "rfi", "nfi", "ifi", "rni", 
      "gfi", "agfi", "mfi", "rmsea", "rmsealo", "rmseaup", 
      "srmr", "srmrben", "srmrbnm",
      "npar", "fmin", "cn05", "cn01", "ave", "cr"
    )
    
    measure_map <- c(
      chisq = "chisq", df = "df", pvalue = "pvalue", cfi = "cfi", tli = "tli", 
      nnfi = "nnfi", rfi = "rfi", nfi = "nfi", ifi = "ifi", rni = "rni", 
      gfi = "gfi", agfi = "agfi", mfi = "mfi", 
      rmsea = "rmsea", rmsealo = "rmsea.ci.lower", rmseaup = "rmsea.ci.upper", 
      srmr = "srmr", srmrben = "srmr_bentler", srmrbnm = "srmr_bentler_nomean", 
      npar = "npar", fmin = "fmin", cn05 = "cn_05", cn01 = "cn_01",
      ave = "ave", cr = "cr"
    )
    
    robust_map <- c(
      chisq = "chisq.scaled", df = "df.scaled", pvalue = "pvalue.scaled",
      cfi = "cfi.robust", tli = "tli.robust", nnfi = "nnfi.robust", 
      nfi = "nfi.scaled", ifi = "ifi.scaled", rni = "rni.robust",
      rmsea = "rmsea.robust", rmsealo = "rmsea.ci.lower.robust", rmseaup = "rmsea.ci.upper.robust"
    )
    
    selected_measures <- switch(fitindices,
                                "short" = c("chisq", "df", "pvalue", "cfi", "rmsea", "srmr", "ave", "cr"),
                                "long" = c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsealo", "rmseaup", "srmr", "ave", "cr"),
                                "full" = fit_measures
    )
    
    fit_table <- sapply(estimators, function(est) {
      if (est %in% names(spec_result$results)) {
        sapply(selected_measures, function(m) {
          if (est %in% c("MLR", "ULSM", "ULSMV") && m %in% names(robust_map)) {
            m_robust <- robust_map[m]
            if (m_robust %in% names(spec_result$results[[est]]$fit_indices)) {
              value <- spec_result$results[[est]]$fit_indices[m_robust]
            } else {
              return(sprintf("%10s", "NA"))
            }
          } else if (measure_map[m] %in% names(spec_result$results[[est]]$fit_indices)) {
            value <- spec_result$results[[est]]$fit_indices[measure_map[m]]
          } else if (m == "ave") {
            # Calculate AVE
            loadings <- spec_result$results[[est]]$loadings$est.std
            value <- mean(loadings^2)
          } else if (m == "cr") {
            # Calculate CR (Composite Reliability)
            loadings <- spec_result$results[[est]]$loadings$est.std
            value <- sum(loadings)^2 / (sum(loadings)^2 + sum(1 - loadings^2))
          } else {
            return(sprintf("%10s", "NA"))
          }
          
          if (m %in% c("cfi", "tli", "nnfi", "rfi", "nfi", "ifi", "rni", "gfi", "agfi", "mfi", "ave", "cr", 
                       "rmsea", "rmsealo", "rmseaup", "srmr", "srmrben", "srmrbnm")) {
            color_code_fit(m, value)
          } else {
            sprintf("%10.3f", value)
          }
        })
      } else {
        rep(sprintf("%10s", "NA"), length(selected_measures))
      }
    })
    rownames(fit_table) <- selected_measures
    
    truncated_estimators <- sapply(estimators, truncate_name)
    
    cat(sprintf("%-8s", ""))
    cat(paste(sprintf("%10s", truncated_estimators), collapse = ""), "\n")
    for (i in 1:nrow(fit_table)) {
      cat(sprintf("%-8s", rownames(fit_table)[i]))
      cat(paste(fit_table[i,], collapse = ""), "\n")
    }
    
    # 12. Print loadings
    if (loadings == "unstd" || loadings == "both") {
      cat("\nUnstandardized Loadings:\n")
      unstd_loading_table <- matrix(NA, nrow = length(spec_result$scale_items), ncol = length(estimators))
      colnames(unstd_loading_table) <- estimators
      rownames(unstd_loading_table) <- spec_result$scale_items
      
      for (est in names(spec_result$results)) {
        est_loadings <- spec_result$results[[est]]$loadings
        unstd_loading_table[est_loadings$rhs, est] <- est_loadings$est
      }
      
      # Truncate variable names
      truncated_varnames <- sapply(rownames(unstd_loading_table), function(name) {
        if (nchar(name) > 8) {
          paste0(substr(name, 1, 3), "..", substr(name, nchar(name)-2, nchar(name)))
        } else {
          name
        }
      })
      
      cat(sprintf("%-8s", ""))
      cat(paste(sprintf("%10s", truncated_estimators), collapse = ""), "\n")
      for (i in 1:nrow(unstd_loading_table)) {
        cat(sprintf("%-8s", truncated_varnames[i]))
        for (j in 1:ncol(unstd_loading_table)) {
          cat(sprintf("%10.3f", unstd_loading_table[i,j]))
        }
        cat("\n")
      }
    }
    
    if (loadings == "std" || loadings == "both") {
      cat("\nStandardized Loadings:\n")
      std_loading_table <- matrix(NA, nrow = length(spec_result$scale_items), ncol = length(estimators))
      colnames(std_loading_table) <- estimators
      rownames(std_loading_table) <- spec_result$scale_items
      
      for (est in names(spec_result$results)) {
        est_loadings <- spec_result$results[[est]]$loadings
        std_loading_table[est_loadings$rhs, est] <- est_loadings$est.std
      }
      
      # Calculate row averages and sort
      row_avgs <- rowMeans(std_loading_table, na.rm = TRUE)
      sorted_indices <- order(abs(row_avgs), decreasing = TRUE)
      std_loading_table <- std_loading_table[sorted_indices, ]
      
      # Truncate variable names
      truncated_varnames <- sapply(rownames(std_loading_table), function(name) {
        if (nchar(name) > 8) {
          paste0(substr(name, 1, 3), "..", substr(name, nchar(name)-2, nchar(name)))
        } else {
          name
        }
      })
      
      cat(sprintf("%-8s", ""))
      cat(paste(sprintf("%10s", truncated_estimators), collapse = ""), "\n")
      for (i in 1:nrow(std_loading_table)) {
        cat(sprintf("%-8s", truncated_varnames[i]))
        for (j in 1:ncol(std_loading_table)) {
          cat(color_gradient(std_loading_table[i,j]))
        }
        cat("\n")
      }
    }
    
    # Print variable descriptions if available
    if (!is.null(var_descriptions)) {
      cat("\nVariable Descriptions:\n")
      for (item in rownames(std_loading_table)) {
        if (item %in% rownames(var_descriptions)) {
          cat(sprintf("%s: %s\n", item, var_descriptions[item, "vardesc"]))
        }
      }
    }
  }
  
  # 14. Prepare return object
  return_object <- list(
    fit_indices = lapply(all_results, function(x) sapply(x$results, function(y) y$fit_indices)),
    std_loadings = lapply(all_results, function(x) lapply(x$results, function(y) y$loadings)),
    lavaan_models = lapply(all_results, function(x) lapply(x$results, function(y) y$cfa_fit))
  )
  
  invisible(return_object)
}


mod <- ''



cfawiz_multiest(idf, "regid", fitindices = "full", loadings = "both", colored = TRUE)
cfawiz_multiest(idf, "penvb", fitindices = "full", loadings = "both", colored = TRUE)
cfawiz_multiest(idf, "regid", fitindices = "full", loadings = "std", colored = TRUE, vardesc = "vardesc.csv")


cfawiz(idf,"regid")
cfawiz_multiest(idf,"regid",fitindices="full")



cfawiz_multiest(idf,"regid,place",estimator="MULTI")




cfawiz(idf, "bint", fitindices = "full", loadings = "both", colored = TRUE)
