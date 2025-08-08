# --- MODULE 5: PCI CALCULATION & STATISTICAL ANALYSIS ---
pacman::p_load(sf, terra, dplyr, tidyr, purrr, ggplot2, corrplot, car, MASS)

cat("=== MODULE 5: PCI CALCULATION & STATISTICAL ANALYSIS ===\n")

# --- 5.1: COMBINE ALL BATCH RESULTS ---
cat("--- 5.1: Combining all batch results ---\n")

# Load and combine all sample batch files
batch_files <- list.files("data/silver/PCI/", pattern = "sample_batch_.*\\.rds", full.names = TRUE)
cat(paste("Found", length(batch_files), "batch files\n"))

# Combine all batches into single dataset
all_variables <- map_dfr(batch_files, readRDS)
cat(paste("Combined dataset:", nrow(all_variables), "records\n"))

# Check data structure
cat("Data structure:\n")
print(str(all_variables))

# --- 5.2: CALCULATE ALL 5 PCI METHODS ---
cat("--- 5.2: Calculating PCI using 5 methods ---\n")

# Separate parks and buffers
parks_data <- all_variables %>% filter(geometry_type == "park")
buffers_data <- all_variables %>% filter(geometry_type != "park")

cat(paste("Parks:", nrow(parks_data), "| Buffers:", nrow(buffers_data), "\n"))

# --- PCI METHOD 1: FRM (Fixed Radius Method - 500m) ---
calculate_frm_pci <- function(parks_data, buffers_data) {
  # Use 500m buffer (closest available)
  buffer_500m <- buffers_data %>% 
    filter(buffer_distance == 500) %>%
    select(park_id, lst_buffer_500m = mean_lst)
  
  frm_pci <- parks_data %>%
    select(park_id, lst_park = mean_lst) %>%
    left_join(buffer_500m, by = "park_id") %>%
    mutate(PCI_FRM = lst_buffer_500m - lst_park) %>%
    select(park_id, PCI_FRM)
  
  return(frm_pci)
}

# --- PCI METHOD 2: ERM (Equal Radius Method) ---
calculate_erm_pci <- function(parks_data, buffers_data) {
  # Calculate park radius: R = sqrt(Area/π)
  park_radius <- parks_data %>%
    mutate(park_radius_m = sqrt(area_ha * 10000 / pi)) %>%
    select(park_id, park_radius_m, lst_park = mean_lst)
  
  # Find closest buffer distance for each park
  erm_pci <- park_radius %>%
    left_join(buffers_data, by = "park_id") %>%
    mutate(radius_diff = abs(buffer_distance - park_radius_m)) %>%
    group_by(park_id) %>%
    slice_min(radius_diff, n = 1) %>%
    mutate(PCI_ERM = mean_lst - lst_park) %>%
    select(park_id, PCI_ERM) %>%
    ungroup()
  
  return(erm_pci)
}

# --- PCI METHOD 3: EAM (Equal Area Method) ---
calculate_eam_pci <- function(parks_data, buffers_data) {
  # For EAM, we need buffers with equal area to parks
  # This requires special buffer geometries - using closest buffer as approximation
  park_areas <- parks_data %>%
    select(park_id, park_area_ha = area_ha, lst_park = mean_lst)
  
  # Use buffer that creates approximately equal area (simplified)
  eam_pci <- park_areas %>%
    left_join(buffers_data, by = "park_id") %>%
    mutate(area_diff = abs(area_ha - park_area_ha)) %>%
    group_by(park_id) %>%
    slice_min(area_diff, n = 1) %>%
    mutate(PCI_EAM = mean_lst - lst_park) %>%
    select(park_id, PCI_EAM) %>%
    ungroup()
  
  return(eam_pci)
}

# --- PCI METHOD 4: TPM-M (Turning Point Method - Mean) ---
calculate_tpm_m_pci <- function(parks_data, buffers_data) {
  park_lst <- parks_data %>% select(park_id, lst_park = mean_lst)
  
  # Find turning point (first local minimum in LST profile)
  tpm_m_pci <- buffers_data %>%
    left_join(park_lst, by = "park_id") %>%
    group_by(park_id) %>%
    arrange(buffer_distance) %>%
    # Simple turning point: first buffer where LST starts increasing
    mutate(lst_diff = lead(mean_lst) - mean_lst) %>%
    filter(!is.na(lst_diff)) %>%
    filter(lst_diff > 0 | row_number() == 1) %>%  # First increasing point or first buffer
    slice_head(n = 1) %>%
    mutate(PCI_TPM_M = mean_lst - lst_park) %>%
    select(park_id, PCI_TPM_M, turning_point_distance = buffer_distance) %>%
    ungroup()
  
  return(tpm_m_pci)
}

# --- PCI METHOD 5: TPM-A (Turning Point Method - Accumulated) ---
calculate_tpm_a_pci <- function(parks_data, buffers_data) {
  park_lst <- parks_data %>% select(park_id, lst_park = mean_lst)
  
  # Calculate accumulated temperature reduction
  tpm_a_pci <- buffers_data %>%
    left_join(park_lst, by = "park_id") %>%
    group_by(park_id) %>%
    arrange(buffer_distance) %>%
    mutate(
      temp_reduction = pmax(0, lst_park - mean_lst),  # Only positive reductions
      cumulative_reduction = cumsum(temp_reduction * 30)  # 30m intervals
    ) %>%
    # Find maximum cooling distance
    filter(temp_reduction > 0) %>%
    summarise(
      max_cooling_distance = max(buffer_distance, na.rm = TRUE),
      total_accumulated_reduction = max(cumulative_reduction, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      PCI_TPM_A = ifelse(is.finite(total_accumulated_reduction), 
                         total_accumulated_reduction / max_cooling_distance, 
                         0)
    ) %>%
    select(park_id, PCI_TPM_A, max_cooling_distance)
  
  return(tpm_a_pci)
}

# Calculate all PCI methods
cat("Calculating FRM PCI...\n")
frm_results <- calculate_frm_pci(parks_data, buffers_data)

cat("Calculating ERM PCI...\n") 
erm_results <- calculate_erm_pci(parks_data, buffers_data)

cat("Calculating EAM PCI...\n")
eam_results <- calculate_eam_pci(parks_data, buffers_data)

cat("Calculating TPM-M PCI...\n")
tpm_m_results <- calculate_tpm_m_pci(parks_data, buffers_data)

cat("Calculating TPM-A PCI...\n")
tpm_a_results <- calculate_tpm_a_pci(parks_data, buffers_data)

# --- 5.3: COMBINE PCI RESULTS WITH PARK CHARACTERISTICS ---
cat("--- 5.3: Creating final analysis dataset ---\n")

# Combine all PCI methods
all_pci <- parks_data %>%
  select(park_id, area_ha, perimeter_m, mean_lst, mean_ndvi, 
         FAP, GAP, ISAP, WAP, FWAP, LSI, LPI, SHAPE_MN, PD, ED, AI,
         blue, green, grey) %>%
  left_join(frm_results, by = "park_id") %>%
  left_join(erm_results, by = "park_id") %>%
  left_join(eam_results, by = "park_id") %>%
  left_join(tpm_m_results, by = "park_id") %>%
  left_join(tpm_a_results, by = "park_id")

# Add buffer characteristics (500m buffer as control variables)
buffer_controls <- buffers_data %>%
  filter(buffer_distance == 500) %>%
  select(park_id, 
         Buffer_blue = blue, Buffer_green = green, Buffer_grey = grey,
         Buffer_NTL = mean_ndvi) %>%  # Using NDVI as proxy for night time lights
  mutate(Buffer_RL = 1000)  # Placeholder for road length

final_dataset <- all_pci %>%
  left_join(buffer_controls, by = "park_id") %>%
  # Rename variables to match study
  rename(
    PA = area_ha,           # Park Area
    PP = perimeter_m,       # Park Perimeter  
    NDVI_veg = mean_ndvi    # NDVI of vegetated area
  )

cat(paste("Final dataset:", nrow(final_dataset), "parks with", ncol(final_dataset), "variables\n"))

# --- 5.4: DESCRIPTIVE STATISTICS ---
cat("--- 5.4: Descriptive statistics ---\n")

# PCI distribution across methods
pci_summary <- final_dataset %>%
  select(starts_with("PCI_")) %>%
  pivot_longer(everything(), names_to = "Method", values_to = "PCI") %>%
  group_by(Method) %>%
  summarise(
    n = sum(!is.na(PCI)),
    Mean = mean(PCI, na.rm = TRUE),
    SD = sd(PCI, na.rm = TRUE),
    Min = min(PCI, na.rm = TRUE),
    Max = max(PCI, na.rm = TRUE),
    .groups = "drop"
  )

cat("PCI Summary by Method:\n")
print(pci_summary)

# Park characteristics summary  
park_vars <- c("PA", "PP", "NDVI_veg", "LPI", "LSI", "SHAPE_MN", "PD", "ED", "AI", 
               "FAP", "GAP", "ISAP", "WAP", "FWAP")

park_summary <- final_dataset %>%
  select(all_of(park_vars)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    n = sum(!is.na(Value)),
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE),
    .groups = "drop"
  )

cat("Park Characteristics Summary:\n")
print(park_summary)

# --- 5.5: CORRELATION ANALYSIS ---
cat("--- 5.5: Correlation analysis ---\n")

# Select variables for correlation analysis
correlation_vars <- c("PCI_FRM", "PCI_ERM", "PCI_EAM", "PCI_TPM_M", "PCI_TPM_A",
                      "PA", "PP", "NDVI_veg", "LPI", "LSI", "SHAPE_MN", "PD", "ED", "AI",
                      "FAP", "GAP", "ISAP", "WAP", "FWAP")

# Calculate correlation matrix
cor_data <- final_dataset %>%
  select(all_of(correlation_vars)) %>%
  na.omit()

cor_matrix <- cor(cor_data, use = "complete.obs")

# Plot correlation heatmap
cat("Creating correlation plot...\n")
png("output/correlation_heatmap.png", width = 12, height = 10, units = "in", res = 300)
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.cex = 0.8, tl.col = "black", tl.srt = 45)
dev.off()

# --- 5.6: REGRESSION ANALYSIS ---
cat("--- 5.6: Multiple regression analysis ---\n")

# Function to perform stepwise regression for each PCI method
perform_regression <- function(pci_method, dataset) {
  
  # Prepare formula
  predictors <- c("PA", "PP", "NDVI_veg", "LPI", "LSI", "SHAPE_MN", "PD", "ED", "AI",
                  "FAP", "GAP", "ISAP", "WAP", "FWAP", "Buffer_NTL", "Buffer_RL")
  
  formula_str <- paste(pci_method, "~", paste(predictors, collapse = " + "))
  full_formula <- as.formula(formula_str)
  
  # Remove rows with missing data
  model_data <- dataset %>% 
    select(all_of(c(pci_method, predictors))) %>%
    na.omit()
  
  if(nrow(model_data) < 20) {
    cat(paste("Insufficient data for", pci_method, "\n"))
    return(NULL)
  }
  
  # Full model
  full_model <- lm(full_formula, data = model_data)
  
  # Stepwise selection
  step_model <- stepAIC(full_model, direction = "both", trace = FALSE)
  
  # Return model summary
  return(list(
    method = pci_method,
    model = step_model,
    summary = summary(step_model),
    n_obs = nrow(model_data),
    r_squared = summary(step_model)$r.squared,
    adj_r_squared = summary(step_model)$adj.r.squared
  ))
}

# Run regression for each PCI method
pci_methods <- c("PCI_FRM", "PCI_ERM", "PCI_EAM", "PCI_TPM_M", "PCI_TPM_A")
regression_results <- map(pci_methods, ~perform_regression(.x, final_dataset))
names(regression_results) <- pci_methods

# Print regression summaries
for(method in names(regression_results)) {
  if(!is.null(regression_results[[method]])) {
    cat(paste("\n=== REGRESSION RESULTS:", method, "===\n"))
    print(regression_results[[method]]$summary)
    cat(paste("R-squared:", round(regression_results[[method]]$r_squared, 3), "\n"))
    cat(paste("Adjusted R-squared:", round(regression_results[[method]]$adj_r_squared, 3), "\n"))
  }
}

# --- 5.7: VISUALIZATION ---
cat("--- 5.7: Creating visualizations ---\n")

# PCI distribution plot
pci_plot <- final_dataset %>%
  select(park_id, starts_with("PCI_")) %>%
  pivot_longer(-park_id, names_to = "Method", values_to = "PCI") %>%
  mutate(Method = str_remove(Method, "PCI_")) %>%
  ggplot(aes(x = Method, y = PCI, fill = Method)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "PCI Distribution by Method",
       x = "PCI Method", y = "Park Cooling Intensity (°C)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("output/pci_distribution.png", pci_plot, width = 10, height = 6, dpi = 300)

#