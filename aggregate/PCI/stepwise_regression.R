#################################################################
# MODULE 5B: MULTIVARIATE STEPWISE REGRESSION (STUDY-ALIGNED)
#################################################################

pacman::p_load(dplyr, MASS, car, ggplot2, broom, leaps)

cat("=== MODULE 5B: MULTIVARIATE STEPWISE REGRESSION (STUDY-ALIGNED) ===\n")

# --- FUNCTION 1: Load and Validate Data ---
load_and_validate_data <- function() {
  
  cat("--- Loading and validating data for regression ---\n")
  
  # Load Module 5A results
  module5a_results <- readRDS("data/gold/PCI/module5a_correlation_results.rds")
  
  # Load the main dataset
  data <- module5a_results$setup$data
  
  # Following the study methodology - define variable groups properly
  landscape_vars <- c("PA", "PP", "NDVI_veg", "LSI", "LPI")  # Park landscape characteristics
  buffer_vars <- c("Buffer_blue", "Buffer_green", "Buffer_grey", "Buffer_NTL")  # Buffer environment
  
  # Check data availability and quality
  all_vars <- c("PCI", landscape_vars, buffer_vars)
  available_vars <- intersect(all_vars, names(data))
  
  # Remove outliers (following study methodology)
  pci_mean <- mean(data$PCI, na.rm = TRUE)
  pci_sd <- sd(data$PCI, na.rm = TRUE)
  data_clean <- data[abs(data$PCI - pci_mean) <= 3 * pci_sd, ]
  
  cat(paste("  -> Removed", nrow(data) - nrow(data_clean), "PCI outliers\n"))
  
  # Create regression dataset with complete cases
  reg_data <- data_clean[, available_vars]
  reg_data_complete <- reg_data[complete.cases(reg_data), ]
  
  cat(paste("  -> Final dataset:", nrow(reg_data_complete), "parks\n"))
  cat(paste("  -> Available landscape vars:", sum(landscape_vars %in% names(reg_data_complete)), "\n"))
  cat(paste("  -> Available buffer vars:", sum(buffer_vars %in% names(reg_data_complete)), "\n"))
  
  return(list(
    data = reg_data_complete,
    landscape_vars = intersect(landscape_vars, names(reg_data_complete)),
    buffer_vars = intersect(buffer_vars, names(reg_data_complete)),
    module5a_results = module5a_results
  ))
}

# --- FUNCTION 2: Study-Aligned Stepwise Regression ---
perform_study_aligned_regression <- function(data_setup) {
  
  cat("--- Performing Study-Aligned Stepwise Regression ---\n")
  
  data <- data_setup$data
  landscape_vars <- data_setup$landscape_vars
  buffer_vars <- data_setup$buffer_vars
  
  # Following study methodology: start with all potential predictors
  all_predictors <- c(landscape_vars, buffer_vars)
  
  cat(paste("  -> Total potential predictors:", length(all_predictors), "\n"))
  cat(paste("     Landscape:", paste(landscape_vars, collapse = ", "), "\n"))
  cat(paste("     Buffer:", paste(buffer_vars, collapse = ", "), "\n"))
  
  # Build models following the study approach
  null_model <- lm(PCI ~ 1, data = data)
  full_formula <- as.formula(paste("PCI ~", paste(all_predictors, collapse = " + ")))
  full_model <- lm(full_formula, data = data)
  
  # Study methodology: Use both forward and backward stepwise
  cat("  -> Running forward stepwise selection...\n")
  forward_model <- step(null_model, 
                        scope = list(lower = null_model, upper = full_model),
                        direction = "forward", 
                        trace = 0,
                        k = 2)  # Use AIC (k=2) as per study
  
  cat("  -> Running backward stepwise selection...\n")
  backward_model <- step(full_model, 
                         direction = "backward", 
                         trace = 0,
                         k = 2)
  
  cat("  -> Running bidirectional stepwise selection...\n")
  both_model <- step(null_model,
                     scope = list(lower = null_model, upper = full_model),
                     direction = "both",
                     trace = 0,
                     k = 2)
  
  # Compare models and select best
  models <- list(
    "Forward" = forward_model,
    "Backward" = backward_model, 
    "Both" = both_model
  )
  
  model_comparison <- data.frame(
    Method = names(models),
    AIC = sapply(models, AIC),
    R_squared = sapply(models, function(x) summary(x)$r.squared),
    Adj_R_squared = sapply(models, function(x) summary(x)$adj.r.squared),
    N_predictors = sapply(models, function(x) length(coef(x)) - 1)
  )
  
  cat("\n--- Model Comparison ---\n")
  print(model_comparison)
  
  # Select best model (highest Adjusted R²)
  best_method <- model_comparison$Method[which.max(model_comparison$Adj_R_squared)]
  final_model <- models[[best_method]]
  
  cat(paste("\n  -> Best model:", best_method, "stepwise\n"))
  
  # Model summary
  final_summary <- summary(final_model)
  
  cat("\n--- Final Model Results ---\n")
  cat(sprintf("R² = %.3f\n", final_summary$r.squared))
  cat(sprintf("Adjusted R² = %.3f\n", final_summary$adj.r.squared))
  cat(sprintf("F-statistic = %.2f, p < %s\n", 
              final_summary$fstatistic[1], 
              format.pval(pf(final_summary$fstatistic[1], 
                             final_summary$fstatistic[2], 
                             final_summary$fstatistic[3], 
                             lower.tail = FALSE))))
  
  # Show coefficients
  coef_table <- final_summary$coefficients
  sig_coefs <- coef_table[coef_table[, 4] < 0.05 & rownames(coef_table) != "(Intercept)", ]
  
  cat(sprintf("\nSignificant predictors (%d):\n", nrow(sig_coefs)))
  for(i in 1:nrow(sig_coefs)) {
    var_name <- rownames(sig_coefs)[i]
    estimate <- sig_coefs[i, "Estimate"]
    p_value <- sig_coefs[i, "Pr(>|t|)"]
    significance <- ifelse(p_value < 0.001, "***", 
                           ifelse(p_value < 0.01, "**", 
                                  ifelse(p_value < 0.05, "*", "")))
    cat(sprintf("  %-15s: β = %8.4f, p = %8.4f %s\n", 
                var_name, estimate, p_value, significance))
  }
  
  return(list(
    final_model = final_model,
    model_comparison = model_comparison,
    best_method = best_method,
    summary = final_summary,
    all_models = models
  ))
}

# --- FUNCTION 3: Enhanced Model Validation ---
perform_enhanced_validation <- function(regression_results, data_setup) {
  
  cat("--- Enhanced Model Validation ---\n")
  
  model <- regression_results$final_model
  data <- data_setup$data
  
  # 1. Check assumptions
  validation_results <- list()
  
  # Residuals analysis
  residuals <- residuals(model)
  fitted_vals <- fitted(model)
  
  # Normality test
  shapiro_test <- shapiro.test(sample(residuals, min(5000, length(residuals))))
  validation_results$normality_p <- shapiro_test$p.value
  
  if(shapiro_test$p.value > 0.05) {
    cat("Residuals appear normally distributed (Shapiro-Wilk p > 0.05)\n")
  } else {
    cat(" Residuals may not be normally distributed (Shapiro-Wilk p < 0.05)\n")
  }
  
  # Homoscedasticity (Breusch-Pagan test)
  tryCatch({
    bp_test <- bptest(model)
    validation_results$homoscedasticity_p <- bp_test$p.value
    
    if(bp_test$p.value > 0.05) {
      cat("Homoscedasticity assumption met (BP test p > 0.05)\n")
    } else {
      cat(" Heteroscedasticity detected (BP test p < 0.05)\n")
    }
  }, error = function(e) {
    cat("  -> Could not perform Breusch-Pagan test\n")
  })
  
  # Multicollinearity
  if(length(coef(model)) > 2) {
    vif_values <- vif(model)
    max_vif <- max(vif_values)
    validation_results$max_vif <- max_vif
    
    if(max_vif < 5) {
      cat("No multicollinearity issues (max VIF < 5)\n")
    } else if(max_vif < 10) {
      cat(" Moderate multicollinearity (max VIF 5-10)\n")
    } else {
      cat("High multicollinearity (max VIF > 10)\n")
    }
  }
  
  # Model fit visualization
  create_diagnostic_plots(model, "outputs/enhanced_diagnostics.png")
  
  return(validation_results)
}

# --- FUNCTION 4: Create Diagnostic Plots ---
create_diagnostic_plots <- function(model, filename) {
  
  png(filename, width = 14, height = 10, units = "in", res = 300)
  par(mfrow = c(2, 3))
  
  # Standard diagnostic plots
  plot(model, which = 1, main = "Residuals vs Fitted")
  plot(model, which = 2, main = "Q-Q Plot") 
  plot(model, which = 3, main = "Scale-Location")
  plot(model, which = 5, main = "Residuals vs Leverage")
  
  # Additional plots
  residuals <- residuals(model)
  fitted_vals <- fitted(model)
  
  # Histogram of residuals
  hist(residuals, breaks = 20, main = "Histogram of Residuals", 
       xlab = "Residuals", col = "lightblue")
  
  # Predicted vs Actual
  actual <- model$model[,1]  # First column is dependent variable
  plot(actual, fitted_vals, main = "Predicted vs Actual PCI",
       xlab = "Actual PCI", ylab = "Predicted PCI", 
       pch = 16, alpha = 0.6)
  abline(0, 1, col = "red", lty = 2)
  
  dev.off()
  cat(paste("  -> Enhanced diagnostic plots saved:", filename, "\n"))
}

# --- MAIN EXECUTION ---
main_stepwise_regression_analysis <- function() {
  
  cat("Stepwise Regression\n")
  start_time <- Sys.time()
  
  tryCatch({
    # Step 1: Load and validate data
    data_setup <- load_and_validate_data()
    
    # Step 2: Perform study-aligned regression
    regression_results <- perform_study_aligned_regression(data_setup)
    
    # Step 3: Enhanced validation  
    validation_results <- perform_enhanced_validation(regression_results, data_setup)
    
    # Step 4: Save comprehensive results
    final_results <- list(
      data_setup = data_setup,
      regression_results = regression_results,
      validation_results = validation_results,
      analysis_date = Sys.time()
    )
    
    dir.create("data/gold/PCI", recursive = TRUE, showWarnings = FALSE)
    saveRDS(final_results, "data/gold/PCI/module5b_study_aligned_results.rds")
    
    # Export results
    dir.create("outputs", showWarnings = FALSE)
    
    # Export coefficients
    coef_df <- broom::tidy(regression_results$final_model)
    write.csv(coef_df, "outputs/final_regression_coefficients.csv", row.names = FALSE)
    
    # Export model comparison
    write.csv(regression_results$model_comparison, "outputs/stepwise_model_comparison.csv", row.names = FALSE)
    
    total_time <- difftime(Sys.time(), start_time, units = "mins")
    
    cat("\n=== MODULE 5B STUDY-ALIGNED COMPLETE ===\n")
    cat(paste("Processing time:", round(total_time, 2), "minutes\n"))
    cat(paste("Final R² =", round(regression_results$summary$r.squared, 3), "\n"))
    cat(paste("Adjusted R² =", round(regression_results$summary$adj.r.squared, 3), "\n"))
    cat(paste("Best method:", regression_results$best_method, "stepwise\n"))
    
    significant_predictors <- sum(regression_results$summary$coefficients[,4] < 0.05) - 1
    cat(paste("Significant predictors:", significant_predictors, "\n"))
    
    cat("\nMODULE 5B SUCCESS! Study methodology replicated!\n")
    
    return(final_results)
    
  }, error = function(e) {
    cat(paste("ERROR in Module 5B:", e$message, "\n"))
    return(NULL)
  })
}

# --- EXECUTE ---
module5b_results <- main_stepwise_regression_analysis()