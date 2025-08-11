#################################################################
# MODULE 5A: PARTIAL CORRELATION ANALYSIS (CLEANED)
#################################################################

pacman::p_load(sf, terra, dplyr, corrplot, ppcor, ggplot2, reshape2)

cat("=== MODULE 5A: PARTIAL CORRELATION ANALYSIS ===\n")

# --- FUNCTION 1: Data Preparation ---
prepare_correlation_data <- function() {
  
  cat("--- Loading and preparing data for correlation analysis ---\n")
  
  # Load park characteristics
  park_data <- readRDS("data/gold/PCI/park_characteristics_ready_for_pci.rds")
  
  # Check for PCI
  if(!"PCI" %in% names(park_data)) {
    stop("PCI not found in dataset - run Module 4 first")
  }
  
  # Remove rows with missing PCI
  analysis_data <- park_data[!is.na(park_data$PCI), ]
  
  cat(paste("  -> Dataset:", nrow(analysis_data), "parks with valid PCI\n"))
  cat(paste("  -> PCI range:", paste(round(range(analysis_data$PCI, na.rm = TRUE), 3), collapse = " to "), "°C\n"))
  
  # Define variable groups
  landscape_vars <- c("PA", "PP", "NDVI_veg", "LSI", "LPI")
  buffer_vars <- c("Buffer_blue", "Buffer_green", "Buffer_grey")
  control_vars <- c("Buffer_NTL", "Buffer_RL")
  
  # Keep only variables with sufficient data and variance
  all_vars <- c(landscape_vars, buffer_vars, control_vars)
  clean_vars <- c()
  
  for(var in all_vars) {
    if(var %in% names(analysis_data)) {
      var_data <- analysis_data[[var]][!is.na(analysis_data[[var]])]
      
      # Check if variable has sufficient data and variance
      if(length(var_data) >= nrow(analysis_data) * 0.5 &&  # At least 50% non-missing
         length(unique(var_data)) > 1 &&                   # More than 1 unique value
         sd(var_data, na.rm = TRUE) > 0) {                 # Non-zero variance
        clean_vars <- c(clean_vars, var)
      } else {
        cat(paste("    -> Removing", var, ": insufficient data or variance\n"))
      }
    }
  }
  
  # Categorize clean variables
  available_landscape <- intersect(landscape_vars, clean_vars)
  available_buffer <- intersect(buffer_vars, clean_vars)
  available_control <- intersect(control_vars, clean_vars)
  
  cat(paste("  -> Landscape variables:", length(available_landscape), "\n"))
  cat(paste("  -> Buffer variables:", length(available_buffer), "\n"))
  cat(paste("  -> Control variables:", length(available_control), "\n"))
  
  return(list(
    data = analysis_data,
    landscape_vars = available_landscape,
    buffer_vars = available_buffer,
    control_vars = available_control,
    all_explanatory = c(available_landscape, available_buffer),
    clean_vars = clean_vars
  ))
}

# --- FUNCTION 2: Pearson Correlation ---
perform_pearson_correlation <- function(analysis_setup) {
  
  cat("--- Performing Pearson Correlation Analysis ---\n")
  
  data <- analysis_setup$data
  all_vars <- c("PCI", analysis_setup$clean_vars)
  
  # Create correlation dataset
  corr_data <- data[, all_vars]
  
  # Calculate Pearson correlation matrix
  pearson_matrix <- cor(corr_data, use = "pairwise.complete.obs", method = "pearson")
  
  # Extract PCI correlations
  pci_correlations <- pearson_matrix["PCI", ]
  pci_correlations <- pci_correlations[names(pci_correlations) != "PCI"]
  pci_correlations_sorted <- sort(abs(pci_correlations), decreasing = TRUE)
  
  cat("\n--- Top 10 Pearson Correlations with PCI ---\n")
  top_correlations <- head(pci_correlations_sorted, 10)
  for(i in 1:length(top_correlations)) {
    var_name <- names(top_correlations)[i]
    corr_value <- pci_correlations[var_name]
    cat(sprintf("%2d. %-15s: r = %6.3f\n", i, var_name, corr_value))
  }
  
  # Create correlation plot
  dir.create("outputs", showWarnings = FALSE)
  if(ncol(pearson_matrix) <= 20) {
    png("outputs/pearson_correlation_matrix.png", width = 12, height = 10, units = "in", res = 300)
    corrplot(pearson_matrix, method = "color", type = "upper", order = "hclust",
             tl.cex = 0.8, tl.col = "black", tl.srt = 45,
             title = "Pearson Correlation Matrix", mar = c(0,0,1,0))
    dev.off()
    cat("  -> Pearson correlation plot saved\n")
  }
  
  return(list(
    correlation_matrix = pearson_matrix,
    pci_correlations = pci_correlations,
    clean_data = corr_data,
    significant_vars = names(top_correlations)
  ))
}

# --- FUNCTION 3: Partial Correlation ---
perform_partial_correlation <- function(analysis_setup, pearson_results) {
  
  cat("--- Performing Partial Correlation Analysis ---\n")
  
  control_vars <- analysis_setup$control_vars
  explanatory_vars <- analysis_setup$all_explanatory
  
  # Check if we have control variables
  available_controls <- intersect(control_vars, names(pearson_results$clean_data))
  if(length(available_controls) == 0) {
    cat("  -> No control variables available, using Pearson correlations\n")
    return(list(
      partial_correlations = pearson_results$pci_correlations,
      method_used = "pearson",
      control_vars_used = NULL
    ))
  }
  
  cat(paste("  -> Control variables:", paste(available_controls, collapse = ", "), "\n"))
  
  # Use complete cases for partial correlation
  partial_data_complete <- pearson_results$clean_data[complete.cases(pearson_results$clean_data), ]
  
  cat(paste("  -> Complete cases:", nrow(partial_data_complete), "\n"))
  
  if(nrow(partial_data_complete) < 30) {
    cat("  -> Too few complete cases, using Pearson correlations\n")
    return(list(
      partial_correlations = pearson_results$pci_correlations,
      method_used = "pearson_insufficient_data",
      control_vars_used = available_controls
    ))
  }
  
  # Calculate partial correlations
  partial_results <- list()
  explanatory_in_data <- intersect(explanatory_vars, names(partial_data_complete))
  
  successful_count <- 0
  
  for(var in explanatory_in_data) {
    tryCatch({
      # Calculate partial correlation
      pcor_result <- pcor.test(
        x = partial_data_complete[["PCI"]],
        y = partial_data_complete[[var]],
        z = partial_data_complete[, available_controls, drop = FALSE],
        method = "pearson"
      )
      
      if(!is.na(pcor_result$estimate) && !is.infinite(pcor_result$estimate)) {
        partial_results[[var]] <- list(
          correlation = pcor_result$estimate,
          p_value = pcor_result$p.value
        )
        successful_count <- successful_count + 1
      }
    }, error = function(e) {
      cat(paste("    -> Skipped", var, ": calculation failed\n"))
    })
  }
  
  cat(paste("  -> Calculated partial correlations for", successful_count, "variables\n"))
  
  if(successful_count == 0) {
    cat("  -> No partial correlations calculated, using Pearson correlations\n")
    return(list(
      partial_correlations = pearson_results$pci_correlations,
      method_used = "pearson_partial_failed",
      control_vars_used = available_controls
    ))
  }
  
  # Extract results
  partial_correlations <- sapply(partial_results, function(x) x$correlation)
  partial_p_values <- sapply(partial_results, function(x) x$p_value)
  
  partial_correlations_sorted <- sort(abs(partial_correlations), decreasing = TRUE)
  
  cat(paste("\n--- Top", min(10, length(partial_correlations_sorted)), "Partial Correlations with PCI ---\n"))
  cat(paste("(controlling for", paste(available_controls, collapse = ", "), ")\n"))
  
  top_partial <- head(partial_correlations_sorted, min(10, length(partial_correlations_sorted)))
  for(i in 1:length(top_partial)) {
    var_name <- names(top_partial)[i]
    corr_value <- partial_correlations[var_name]
    p_value <- partial_p_values[var_name]
    significance <- ifelse(p_value < 0.001, "***",
                           ifelse(p_value < 0.01, "**",
                                  ifelse(p_value < 0.05, "*", "")))
    cat(sprintf("%2d. %-15s: r = %6.3f, p = %6.3f %s\n", i, var_name, corr_value, p_value, significance))
  }
  
  return(list(
    partial_correlations = partial_correlations,
    p_values = partial_p_values,
    method_used = "partial",
    control_vars_used = available_controls,
    significant_vars = names(partial_correlations)[partial_p_values < 0.05]
  ))
}

# --- FUNCTION 4: Comparison Plot ---
create_correlation_comparison <- function(pearson_results, partial_results) {
  
  cat("--- Creating correlation comparison ---\n")
  
  if(partial_results$method_used != "partial") {
    cat("  -> Skipping comparison: partial correlation not available\n")
    return(NULL)
  }
  
  # Get common variables
  common_vars <- intersect(names(pearson_results$pci_correlations),
                           names(partial_results$partial_correlations))
  
  if(length(common_vars) == 0) {
    cat("  -> No common variables for comparison\n")
    return(NULL)
  }
  
  comparison_data <- data.frame(
    Variable = common_vars,
    Pearson = pearson_results$pci_correlations[common_vars],
    Partial = partial_results$partial_correlations[common_vars],
    P_value = partial_results$p_values[common_vars],
    Significant = partial_results$p_values[common_vars] < 0.05,
    stringsAsFactors = FALSE
  )
  
  # Create comparison plot
  plot_data <- melt(comparison_data[, c("Variable", "Pearson", "Partial")],
                    id.vars = "Variable",
                    variable.name = "Method", value.name = "Correlation")
  
  p <- ggplot(plot_data, aes(x = reorder(Variable, abs(Correlation)),
                             y = Correlation, fill = Method)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
    coord_flip() +
    labs(title = "Pearson vs Partial Correlations with PCI",
         subtitle = paste("Partial correlations control for:",
                          paste(partial_results$control_vars_used, collapse = ", ")),
         x = "Variables", y = "Correlation with PCI") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 12),
          plot.subtitle = element_text(hjust = 0.5, size = 10)) +
    scale_fill_manual(values = c("Pearson" = "lightblue", "Partial" = "darkblue"))
  
  ggsave("outputs/correlation_comparison.png", p, width = 12, height = 8, dpi = 300)
  cat("  -> Correlation comparison plot saved\n")
  
  return(comparison_data)
}

# --- MAIN EXECUTION ---
main_partial_correlation_analysis <- function() {
  
  cat("🎯 Starting Module 5A: Partial Correlation Analysis\n")
  start_time <- Sys.time()
  
  tryCatch({
    # Step 1: Data preparation
    analysis_setup <- prepare_correlation_data()
    
    # Step 2: Pearson correlation
    pearson_results <- perform_pearson_correlation(analysis_setup)
    
    # Step 3: Partial correlation
    partial_results <- perform_partial_correlation(analysis_setup, pearson_results)
    
    # Step 4: Create comparison
    comparison_data <- create_correlation_comparison(pearson_results, partial_results)
    
    # Save results
    results <- list(
      setup = analysis_setup,
      pearson = pearson_results,
      partial = partial_results,
      comparison = comparison_data,
      analysis_date = Sys.time()
    )
    
    dir.create("data/gold/PCI", recursive = TRUE, showWarnings = FALSE)
    saveRDS(results, "data/gold/PCI/module5a_correlation_results.rds")
    
    # Export results to CSV
    dir.create("outputs", showWarnings = FALSE)
    
    # Export Pearson correlations
    pearson_df <- data.frame(
      Variable = names(pearson_results$pci_correlations),
      Pearson_r = round(pearson_results$pci_correlations, 4),
      Abs_r = round(abs(pearson_results$pci_correlations), 4),
      row.names = NULL
    )
    pearson_df <- pearson_df[order(pearson_df$Abs_r, decreasing = TRUE), ]
    write.csv(pearson_df, "outputs/pearson_correlations.csv", row.names = FALSE)
    
    # Export partial correlations
    if(partial_results$method_used == "partial") {
      partial_df <- data.frame(
        Variable = names(partial_results$partial_correlations),
        Partial_r = round(partial_results$partial_correlations, 4),
        p_value = round(partial_results$p_values, 4),
        Significant = partial_results$p_values < 0.05,
        Abs_r = round(abs(partial_results$partial_correlations), 4),
        row.names = NULL
      )
      partial_df <- partial_df[order(partial_df$Abs_r, decreasing = TRUE), ]
      write.csv(partial_df, "outputs/partial_correlations.csv", row.names = FALSE)
    }
    
    total_time <- difftime(Sys.time(), start_time, units = "mins")
    
    cat("\n=== MODULE 5A COMPLETE ===\n")
    cat(paste("Processing time:", round(total_time, 2), "minutes\n"))
    cat(paste("Parks analyzed:", nrow(analysis_setup$data), "\n"))
    cat(paste("Variables used:", length(analysis_setup$clean_vars), "\n"))
    
    if(partial_results$method_used == "partial") {
      significant_count <- sum(partial_results$p_values < 0.05, na.rm = TRUE)
      cat(paste("Significant partial correlations (p<0.05):", significant_count, "\n"))
    }
    
    cat("\n✅ MODULE 5A SUCCESS! Ready for Module 5B: Stepwise Regression\n")
    
    return(results)
    
  }, error = function(e) {
    cat(paste("❌ ERROR in Module 5A:", e$message, "\n"))
    return(NULL)
  })
}

# --- EXECUTE MODULE 5A ---
cat("🚀 Executing Module 5A: Partial Correlation Analysis...\n\n")
module5a_results <- main_partial_correlation_analysis()

# --- VALIDATION ---
if(!is.null(module5a_results)) {
  cat("\n🔍 Final validation:\n")
  
  if(!is.null(module5a_results$pearson)) {
    pearson_count <- length(module5a_results$pearson$pci_correlations)
    cat(paste("✅ Pearson correlations:", pearson_count, "variables\n"))
  }
  
  if(module5a_results$partial$method_used == "partial") {
    partial_count <- length(module5a_results$partial$partial_correlations)
    sig_count <- sum(module5a_results$partial$p_values < 0.05, na.rm = TRUE)
    cat(paste("✅ Partial correlations:", partial_count, "variables,", sig_count, "significant\n"))
  }
  
  cat("\n🎉 MODULE 5A SUCCESS! Ready for Module 5B.\n")
} else {
  cat("\n❌ Module 5A failed. Check error messages above.\n")
}