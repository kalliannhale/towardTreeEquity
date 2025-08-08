# --- 4.8: COMPLETE Variable Calculator with Landscape Metrics (300 PARK SAMPLE) ---
pacman::p_load(sf, terra, dplyr, exactextractr, landscapemetrics, purrr)

# --- FIXED Complete Variable Calculator ---
calculate_fixed_complete <- function(geometry_row, lst_raster, ndvi_raster, land_cover_raster) {
  
  park_id <- geometry_row$park_id
  geometry_type <- geometry_row$geometry_type  
  buffer_distance <- geometry_row$buffer_distance
  geom <- st_geometry(geometry_row)
  
  # Basic variables
  area_ha <- as.numeric(st_area(geom)) / 10000
  perimeter_m <- as.numeric(sf::st_perimeter(geom))
  
  results <- data.frame(
    park_id = park_id,
    geometry_type = geometry_type,
    buffer_distance = buffer_distance,
    area_ha = area_ha,
    perimeter_m = perimeter_m,
    mean_lst = exact_extract(lst_raster, geom, 'mean'),
    mean_ndvi = exact_extract(ndvi_raster, geom, 'mean'),
    stringsAsFactors = FALSE
  )
  
  # Land cover class counts and proportions
  lc_values <- exact_extract(land_cover_raster, geom)[[1]]
  if(!is.null(lc_values) && nrow(lc_values) > 0) {
    lc_counts <- table(lc_values$value)
    total_pixels <- sum(lc_counts)
    
    # Calculate proportions for each class
    results$prop_tree_canopy <- ifelse("1" %in% names(lc_counts), lc_counts["1"] / total_pixels, 0)
    results$prop_grass_shrub <- ifelse("2" %in% names(lc_counts), lc_counts["2"] / total_pixels, 0)  
    results$prop_bare_soil <- ifelse("3" %in% names(lc_counts), lc_counts["3"] / total_pixels, 0)
    results$prop_water <- ifelse("4" %in% names(lc_counts), lc_counts["4"] / total_pixels, 0)
    results$prop_buildings <- ifelse("5" %in% names(lc_counts), lc_counts["5"] / total_pixels, 0)
    results$prop_roads <- ifelse("6" %in% names(lc_counts), lc_counts["6"] / total_pixels, 0)
    results$prop_other_impervious <- ifelse("7" %in% names(lc_counts), lc_counts["7"] / total_pixels, 0)
    results$prop_railroads <- ifelse("8" %in% names(lc_counts), lc_counts["8"] / total_pixels, 0)
    
    # Study categories (blue, green, grey)
    results$blue <- results$prop_water  
    results$green <- results$prop_tree_canopy + results$prop_grass_shrub  
    results$grey <- results$prop_bare_soil + results$prop_buildings + results$prop_roads + 
      results$prop_other_impervious + results$prop_railroads  
    
    # PLAND variables
    results$FAP <- results$green  
    results$GAP <- results$prop_grass_shrub    
    results$ISAP <- results$prop_buildings + results$prop_roads + results$prop_other_impervious + results$prop_railroads  
    results$WAP <- results$prop_water  
    results$FWAP <- results$prop_tree_canopy + results$prop_water  
    
    # Landscape metrics (FIXED with proper terra conversion)
    tryCatch({
      # Convert sf to SpatVector for terra
      geom_vect <- vect(geometry_row)
      
      # Crop and mask
      geom_raster <- crop(land_cover_raster, geom_vect)
      geom_raster <- mask(geom_raster, geom_vect)
      
      # Calculate landscape metrics
      results$LSI <- lsm_l_lsi(geom_raster)$value[1]  
      results$LPI <- lsm_l_lpi(geom_raster)$value[1]  
      results$SHAPE_MN <- lsm_l_shape_mn(geom_raster)$value[1]  
      results$PD <- lsm_l_pd(geom_raster)$value[1]   
      results$ED <- lsm_l_ed(geom_raster)$value[1]   
      results$AI <- lsm_l_ai(geom_raster)$value[1]   
      
    }, error = function(e) {
      # Fill with NAs if landscape metrics fail
      results$LSI <<- NA
      results$LPI <<- NA
      results$SHAPE_MN <<- NA
      results$PD <<- NA
      results$ED <<- NA
      results$AI <<- NA
    })
    
  } else {
    # Fill with NAs if no land cover data
    results[c("prop_tree_canopy", "prop_grass_shrub", "prop_bare_soil", "prop_water", 
              "prop_buildings", "prop_roads", "prop_other_impervious", "prop_railroads",
              "blue", "green", "grey", "FAP", "GAP", "ISAP", "WAP", "FWAP",
              "LSI", "LPI", "SHAPE_MN", "PD", "ED", "AI")] <- NA
  }
  
  return(results)
}

# --- LOAD DATA & CREATE 300-PARK SAMPLE ---
cat("--- Loading data and creating 300-park sample ---\n")
all_geometries <- sf::st_read("data/silver/PCI/park_buffer_geometries_tpm_m.gpkg")

# Sample 300 parks
unique_parks <- unique(all_geometries$park_id)
total_parks <- length(unique_parks)
sample_size <- min(300, total_parks)

set.seed(123)  # For reproducibility
sampled_park_ids <- sample(unique_parks, sample_size)

# Filter to sampled parks only
sampled_geometries <- all_geometries[all_geometries$park_id %in% sampled_park_ids, ]

cat(paste("Total parks available:", total_parks, "\n"))
cat(paste("Sampled", sample_size, "parks\n"))
cat(paste("Sample contains", nrow(sampled_geometries), "geometries total\n"))

# Check sample composition
sample_summary <- sampled_geometries %>% 
  group_by(geometry_type) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))
print(sample_summary)

# --- BATCH PROCESSING SETUP ---
batch_size <- 50
output_dir <- "data/silver/PCI/"
if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

sample_total <- nrow(sampled_geometries)
sample_batches <- ceiling(sample_total / batch_size)

cat(paste("Processing", sample_total, "geometries in", sample_batches, "batches\n"))
cat(paste("Estimated time: ~", round(sample_batches * 0.5, 1), "minutes\n"))

# Function to process a batch
process_batch <- function(batch_num, batch_size, geometries_data, lst_raster, ndvi_raster, land_cover_raster, output_dir) {
  
  start_idx <- (batch_num - 1) * batch_size + 1
  end_idx <- min(batch_num * batch_size, nrow(geometries_data))
  
  cat(paste("Processing batch", batch_num, "- geometries", start_idx, "to", end_idx, "\n"))
  
  # Extract batch
  batch_geoms <- geometries_data[start_idx:end_idx, ]
  
  # Process batch
  batch_results <- map_dfr(1:nrow(batch_geoms), function(i) {
    if(i %% 10 == 0) cat(paste("  Geometry", i, "of", nrow(batch_geoms), "\n"))
    calculate_fixed_complete(batch_geoms[i, ], lst_raster, ndvi_raster, land_cover_raster)
  })
  
  # Save batch results
  batch_file <- file.path(output_dir, paste0("sample_batch_", sprintf("%04d", batch_num), ".rds"))
  saveRDS(batch_results, batch_file)
  
  cat(paste("Saved batch", batch_num, "to", batch_file, "\n"))
  
  # Clean up memory
  gc()
  
  return(nrow(batch_results))
}

# --- PROCESS ALL SAMPLE BATCHES ---
start_time <- Sys.time()

for(batch_num in 1:sample_batches) {
  batch_start <- Sys.time()
  
  # Process batch using sampled_geometries
  processed_count <- process_batch(batch_num, batch_size, sampled_geometries, 
                                   lst_raster, ndvi_raster, land_cover_raster, output_dir)
  
  batch_time <- difftime(Sys.time(), batch_start, units = "mins")
  elapsed_time <- difftime(Sys.time(), start_time, units = "mins")
  
  # Progress update
  cat(paste("✅ Batch", batch_num, "of", sample_batches, "complete"))
  cat(paste("(", round(batch_time, 2), "min,", round(elapsed_time, 2), "min total)\n"))
  
  # Memory cleanup every 20 batches
  if(batch_num %% 20 == 0) {
    cat("🧹 Memory cleanup...\n")
    gc()
  }
}

total_time <- difftime(Sys.time(), start_time, units = "mins")
cat(paste("🎉 300-PARK SAMPLE PROCESSING COMPLETE! Total time:", round(total_time, 1), "minutes\n"))

# --- VERIFICATION ---
batch_files <- list.files(output_dir, pattern = "sample_batch_.*\\.rds", full.names = TRUE)
cat(paste("Generated", length(batch_files), "batch files\n"))

# Check first batch results
if(length(batch_files) > 0) {
  first_sample_batch <- readRDS(batch_files[1])
  cat("Sample results summary:\n")
  print(first_sample_batch[1:5, c("park_id", "geometry_type", "buffer_distance", "green", "grey", "LSI", "LPI")])
}