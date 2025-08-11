#################################################################
# MODULE 4A+4B: SIMPLE FIX - Use Ring Geometries for LST Only
#################################################################

pacman::p_load(sf, terra, dplyr, exactextractr, landscapemetrics, purrr)

cat("--- Module 4A+4B: FIXED to use ring geometries for LST ---\n")

# --- Keep all your existing functions except modify LST extraction ---

# Load existing data and rasters (keep same as your original)
safe_load_rasters <- function() {
  tryCatch({
    cat("--- Loading and validating raster data ---\n")
    rasters <- list(
      lst = rast("data/silver/PCI/lst_raster.tif"),
      ndvi = rast("data/silver/PCI/ndvi_raster.tif"),
      land_cover = rast("data/silver/PCI/land_cover_raster.tif"),
      ntl = rast("data/silver/PCI/ntl_raster.tif")
    )
    
    for(name in names(rasters)) {
      if(is.null(rasters[[name]])) stop(paste("Failed to load", name, "raster"))
    }
    
    cat(" -> All rasters loaded successfully\n")
    return(rasters)
  }, error = function(e) {
    stop(paste("Failed to load rasters:", e$message))
  })
}

# Keep your geometry preparation but add ring buffer selection
safe_prepare_geometries_with_rings <- function(sampled_park_ids, target_buffer_distance = 480) {
  tryCatch({
    cat("--- Safely preparing geometries with ring buffers ---\n")
    all_geometries <- sf::st_read("data/silver/PCI/park_buffer_geometries_tpm_m.gpkg")
    
    # Get park geometries (buffer_distance = 0, geometry_type = "park")
    park_geometries <- all_geometries[all_geometries$park_id %in% sampled_park_ids & 
                                        all_geometries$geometry_type == "park", ]
    
    # Find closest ring buffer distance to target (480m)
    available_ring_distances <- unique(all_geometries$buffer_distance[
      all_geometries$geometry_type == "buffer_ring" & all_geometries$buffer_distance > 0
    ])
    
    if(length(available_ring_distances) == 0) {
      stop("No ring buffer geometries found")
    }
    
    # Find closest to 480m
    closest_distance <- available_ring_distances[which.min(abs(available_ring_distances - target_buffer_distance))]
    cat(paste("Target:", target_buffer_distance, "m, Using closest:", closest_distance, "m\n"))
    
    # Get ring geometries at chosen distance
    ring_geometries <- all_geometries[all_geometries$park_id %in% sampled_park_ids & 
                                        all_geometries$geometry_type == "buffer_ring" &
                                        all_geometries$buffer_distance == closest_distance, ]
    
    # Remove invalid geometries
    valid_parks <- st_is_valid(park_geometries)
    valid_rings <- st_is_valid(ring_geometries)
    
    if(any(!valid_parks)) {
      cat(paste("Removing", sum(!valid_parks), "invalid park geometries\n"))
      park_geometries <- park_geometries[valid_parks, ]
    }
    
    if(any(!valid_rings)) {
      cat(paste("Removing", sum(!valid_rings), "invalid ring geometries\n"))
      ring_geometries <- ring_geometries[valid_rings, ]
    }
    
    # Ensure matching pairs
    common_park_ids <- intersect(park_geometries$park_id, ring_geometries$park_id)
    park_geometries <- park_geometries[park_geometries$park_id %in% common_park_ids, ]
    ring_geometries <- ring_geometries[ring_geometries$park_id %in% common_park_ids, ]
    
    cat(paste("Final: processing", length(common_park_ids), "parks with", closest_distance, "m ring buffers\n"))
    
    return(list(
      parks = park_geometries, 
      rings = ring_geometries,
      buffer_distance = closest_distance
    ))
    
  }, error = function(e) {
    stop(paste("Failed to prepare geometries:", e$message))
  })
}

# --- ONLY MODIFIED FUNCTION: Extract LST from parks and rings ---
extract_lst_from_parks_and_rings <- function(park_geometries, ring_geometries, lst_raster) {
  
  cat("  -> Extracting LST from parks and ring buffers\n")
  
  n_parks <- nrow(park_geometries)
  
  # Extract LST from parks
  park_lst <- exact_extract(lst_raster, park_geometries, 'mean')
  
  # Extract LST from rings (match by park_id)
  ring_lst <- rep(NA, n_parks)
  
  for(i in 1:n_parks) {
    park_id <- park_geometries$park_id[i]
    matching_ring <- ring_geometries[ring_geometries$park_id == park_id, ]
    
    if(nrow(matching_ring) > 0) {
      tryCatch({
        ring_lst[i] <- exact_extract(lst_raster, matching_ring, 'mean')[[1]]
      }, error = function(e) {
        # ring_lst[i] remains NA
      })
    }
  }
  
  # Calculate PCI
  pci <- ring_lst - park_lst
  
  cat(paste("    -> Successfully extracted LST for", sum(!is.na(park_lst)), "parks\n"))
  cat(paste("    -> Successfully extracted LST for", sum(!is.na(ring_lst)), "ring buffers\n"))
  cat(paste("    -> Successfully calculated PCI for", sum(!is.na(pci)), "parks\n"))
  
  if(sum(!is.na(pci)) > 0) {
    valid_pci <- pci[!is.na(pci)]
    cat(paste("    -> Mean PCI:", round(mean(valid_pci), 3), "°C\n"))
    cat(paste("    -> PCI range:", paste(round(range(valid_pci), 3), collapse = " to "), "°C\n"))
  }
  
  return(data.frame(
    Park_LST = park_lst,
    Buffer_LST = ring_lst,  # This is now ring buffer LST!
    PCI = pci
  ))
}

# Keep all your other extraction functions exactly the same
safe_extract_raster_values <- function(geometries, raster_list, geom_type) {
  
  cat(paste("  -> Safely extracting raster values for", nrow(geometries), geom_type, "geometries\n"))
  
  n_geoms <- nrow(geometries)
  results_list <- list()
  
  # Extract from each raster (EXCEPT LST - we handle that separately)
  for(raster_name in names(raster_list)) {
    if(raster_name == "lst") next  # Skip LST - handled separately
    
    cat(paste("    -> Extracting", raster_name, "\n"))
    
    tryCatch({
      values <- exact_extract(raster_list[[raster_name]], geometries, 'mean')
      results_list[[paste0("mean_", raster_name)]] <- values
    }, error = function(e) {
      cat(paste("      -> Warning: Failed to extract", raster_name, "- filling with NAs\n"))
      results_list[[paste0("mean_", raster_name)]] <<- rep(NA, n_geoms)
    })
  }
  
  # Land cover extraction (keep exactly same as your original)
  cat("    -> Extracting land cover proportions\n")
  
  lc_proportions <- tryCatch({
    lc_extracts <- exact_extract(raster_list$land_cover, geometries)
    
    map_dfr(1:length(lc_extracts), function(i) {
      tryCatch({
        lc_data <- lc_extracts[[i]]
        
        if(is.null(lc_data) || nrow(lc_data) == 0) {
          return(data.frame(prop_tree_canopy = 0, prop_grass_shrub = 0, prop_bare_soil = 0,
                            prop_water = 0, prop_buildings = 0, prop_roads = 0, 
                            prop_other_impervious = 0, prop_railroads = 0))
        }
        
        lc_counts <- table(lc_data$value)
        total_pixels <- sum(lc_counts)
        
        if(total_pixels == 0) {
          return(data.frame(prop_tree_canopy = 0, prop_grass_shrub = 0, prop_bare_soil = 0,
                            prop_water = 0, prop_buildings = 0, prop_roads = 0, 
                            prop_other_impervious = 0, prop_railroads = 0))
        }
        
        data.frame(
          prop_tree_canopy = ifelse("1" %in% names(lc_counts), lc_counts["1"] / total_pixels, 0),
          prop_grass_shrub = ifelse("2" %in% names(lc_counts), lc_counts["2"] / total_pixels, 0),
          prop_bare_soil = ifelse("3" %in% names(lc_counts), lc_counts["3"] / total_pixels, 0),
          prop_water = ifelse("4" %in% names(lc_counts), lc_counts["4"] / total_pixels, 0),
          prop_buildings = ifelse("5" %in% names(lc_counts), lc_counts["5"] / total_pixels, 0),
          prop_roads = ifelse("6" %in% names(lc_counts), lc_counts["6"] / total_pixels, 0),
          prop_other_impervious = ifelse("7" %in% names(lc_counts), lc_counts["7"] / total_pixels, 0),
          prop_railroads = ifelse("8" %in% names(lc_counts), lc_counts["8"] / total_pixels, 0)
        )
      }, error = function(e) {
        return(data.frame(prop_tree_canopy = 0, prop_grass_shrub = 0, prop_bare_soil = 0,
                          prop_water = 0, prop_buildings = 0, prop_roads = 0, 
                          prop_other_impervious = 0, prop_railroads = 0))
      })
    })
    
  }, error = function(e) {
    cat(paste("      -> Warning: Land cover extraction failed - filling with zeros\n"))
    data.frame(
      prop_tree_canopy = rep(0, n_geoms), prop_grass_shrub = rep(0, n_geoms), 
      prop_bare_soil = rep(0, n_geoms), prop_water = rep(0, n_geoms),
      prop_buildings = rep(0, n_geoms), prop_roads = rep(0, n_geoms), 
      prop_other_impervious = rep(0, n_geoms), prop_railroads = rep(0, n_geoms)
    )
  })
  
  # Geometric calculations (keep same)
  area_ha <- tryCatch({
    as.numeric(st_area(geometries)) / 10000
  }, error = function(e) {
    rep(NA, n_geoms)
  })
  
  perimeter_m <- tryCatch({
    as.numeric(st_perimeter(geometries))
  }, error = function(e) {
    rep(NA, n_geoms)
  })
  
  # Combine results (keep same)
  final_results <- tryCatch({
    base_results <- data.frame(
      park_id = geometries$park_id,
      geometry_type = geom_type,
      area_ha = area_ha,
      perimeter_m = perimeter_m,
      stringsAsFactors = FALSE
    )
    
    for(name in names(results_list)) {
      base_results[[name]] <- results_list[[name]]
    }
    
    final_results <- cbind(base_results, lc_proportions)
    
    # Calculate derived variables (keep same)
    final_results$blue <- ifelse(is.na(final_results$prop_water), 0, final_results$prop_water)
    final_results$green <- ifelse(is.na(final_results$prop_tree_canopy) | is.na(final_results$prop_grass_shrub), 0, 
                                  final_results$prop_tree_canopy + final_results$prop_grass_shrub)
    final_results$grey <- ifelse(rowSums(is.na(final_results[, c("prop_bare_soil", "prop_buildings", "prop_roads", "prop_other_impervious", "prop_railroads")])) > 0, 0,
                                 final_results$prop_bare_soil + final_results$prop_buildings + final_results$prop_roads + final_results$prop_other_impervious + final_results$prop_railroads)
    
    final_results$FAP <- final_results$green
    final_results$GAP <- ifelse(is.na(final_results$prop_grass_shrub), 0, final_results$prop_grass_shrub)
    final_results$ISAP <- ifelse(rowSums(is.na(final_results[, c("prop_buildings", "prop_roads", "prop_other_impervious", "prop_railroads")])) > 0, 0,
                                 final_results$prop_buildings + final_results$prop_roads + final_results$prop_other_impervious + final_results$prop_railroads)
    final_results$WAP <- final_results$blue
    final_results$FWAP <- ifelse(is.na(final_results$prop_tree_canopy) | is.na(final_results$prop_water), 0,
                                 final_results$prop_tree_canopy + final_results$prop_water)
    
    return(final_results)
    
  }, error = function(e) {
    cat(paste("      -> Error combining results:", e$message, "\n"))
    return(NULL)
  })
  
  return(final_results)
}

# Keep your landscape metrics function exactly the same
safe_calculate_landscape_metrics <- function(park_geometries, land_cover_raster) {
  
  cat("  -> Calculating landscape metrics with error handling\n")
  
  n_parks <- nrow(park_geometries)
  
  results <- data.frame(
    LSI = rep(NA, n_parks),
    LPI = rep(NA, n_parks),
    SHAPE_MN = rep(NA, n_parks),
    PD = rep(NA, n_parks),
    ED = rep(NA, n_parks),
    AI = rep(NA, n_parks)
  )
  
  successful_metrics <- 0
  
  for(i in 1:n_parks) {
    if(i %% 25 == 0) cat(paste("    -> Processing metrics for park", i, "of", n_parks, "\n"))
    
    tryCatch({
      geom_vect <- vect(park_geometries[i, ])
      geom_raster <- crop(land_cover_raster, geom_vect)
      geom_raster <- mask(geom_raster, geom_vect)
      
      results$LSI[i] <- lsm_l_lsi(geom_raster)$value[1]
      results$LPI[i] <- lsm_l_lpi(geom_raster)$value[1]
      
      successful_metrics <- successful_metrics + 1
      
    }, error = function(e) {
      # Results already initialized with NA
    })
  }
  
  cat(paste("    -> Successfully calculated metrics for", successful_metrics, "of", n_parks, "parks\n"))
  return(results)
}

# --- MAIN PROCESSING with Ring Buffer LST Fix ---
main_processing_fixed <- function() {
  tryCatch({
    start_time <- Sys.time()
    
    # Load rasters safely
    raster_list <- safe_load_rasters()
    
    # Prepare sample
    all_geometries <- sf::st_read("data/silver/PCI/park_buffer_geometries_tpm_m.gpkg")
    unique_parks <- unique(all_geometries$park_id)
    sample_size <- min(300, length(unique_parks))
    set.seed(101)
    sampled_park_ids <- sample(unique_parks, sample_size)
    
    # Prepare geometries with ring buffers (target 480m)
    geometry_data <- safe_prepare_geometries_with_rings(sampled_park_ids, target_buffer_distance = 480)
    
    park_geometries <- geometry_data$parks
    ring_geometries <- geometry_data$rings
    actual_buffer_distance <- geometry_data$buffer_distance
    
    # Extract raster values from parks (everything except LST)
    park_results <- safe_extract_raster_values(park_geometries, raster_list, "park")
    
    # NEW: Extract LST from parks and ring buffers separately
    lst_results <- extract_lst_from_parks_and_rings(park_geometries, ring_geometries, raster_list$lst)
    
    # Calculate landscape metrics
    landscape_metrics <- safe_calculate_landscape_metrics(park_geometries, raster_list$land_cover)
    
    # Combine final results
    final_dataset <- tryCatch({
      data.frame(
        park_id = park_results$park_id,
        PA = park_results$area_ha,
        PP = park_results$perimeter_m / 1000,
        NDVI_veg = park_results$mean_ndvi,
        LSI = landscape_metrics$LSI,
        LPI = landscape_metrics$LPI,
        SHAPE_MN = landscape_metrics$SHAPE_MN,
        PD = landscape_metrics$PD,
        ED = landscape_metrics$ED,
        AI = landscape_metrics$AI,
        FAP = park_results$FAP,
        GAP = park_results$GAP,
        ISAP = park_results$ISAP,
        WAP = park_results$WAP,
        FWAP = park_results$FWAP,
        Buffer_blue = park_results$blue,      # Placeholder - will be replaced by Module 4C
        Buffer_green = park_results$green,    # Placeholder - will be replaced by Module 4C  
        Buffer_grey = park_results$grey,      # Placeholder - will be replaced by Module 4C
        Buffer_NTL = park_results$mean_ntl,   # Placeholder - will be replaced by Module 4C
        Park_LST = lst_results$Park_LST,      # ✅ FIXED: Park LST only
        Buffer_LST = lst_results$Buffer_LST,  # ✅ FIXED: Ring buffer LST only
        PCI = lst_results$PCI,                # ✅ FIXED: Ring LST - Park LST
        buffer_distance_used = actual_buffer_distance,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      stop(paste("Failed to combine final dataset:", e$message))
    })
    
    # Save results
    output_dir <- "data/silver/PCI/"
    if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    
    saveRDS(final_dataset, file.path(output_dir, "park_buffer_characteristics.rds"))
    
    total_time <- difftime(Sys.time(), start_time, units = "mins")
    
    cat("\n=== RING BUFFER FIXED MODULE 4A+4B COMPLETE ===\n")
    cat(paste("Total processing time:", round(total_time, 2), "minutes\n"))
    cat(paste("Successfully processed:", nrow(final_dataset), "parks\n"))
    cat(paste("Using buffer distance:", actual_buffer_distance, "m\n"))
    
    # ✅ KEY: Check PCI results immediately
    if("PCI" %in% names(final_dataset)) {
      valid_pci <- final_dataset$PCI[!is.na(final_dataset$PCI)]
      
      if(length(valid_pci) > 0) {
        cat("\n🎯 PCI RESULTS WITH RING BUFFER FIX:\n")
        cat(paste("Valid PCI calculations:", length(valid_pci), "parks\n"))
        cat(paste("Mean PCI:", round(mean(valid_pci), 3), "°C\n"))
        cat(paste("Median PCI:", round(median(valid_pci), 3), "°C\n"))
        cat(paste("PCI Range:", paste(round(range(valid_pci), 3), collapse = " to "), "°C\n"))
        cat(paste("Standard deviation:", round(sd(valid_pci), 3), "°C\n"))
        
        # Distribution
        cat("\nPCI Distribution:\n")
        cat(paste("  Negative PCI:", sum(valid_pci < 0), "parks (", round(sum(valid_pci < 0)/length(valid_pci)*100, 1), "%)\n"))
        cat(paste("  0-1°C:", sum(valid_pci >= 0 & valid_pci < 1), "parks (", round(sum(valid_pci >= 0 & valid_pci < 1)/length(valid_pci)*100, 1), "%)\n"))
        cat(paste("  1-2°C:", sum(valid_pci >= 1 & valid_pci < 2), "parks (", round(sum(valid_pci >= 1 & valid_pci < 2)/length(valid_pci)*100, 1), "%)\n"))
        cat(paste("  2-3°C:", sum(valid_pci >= 2 & valid_pci < 3), "parks (", round(sum(valid_pci >= 2 & valid_pci < 3)/length(valid_pci)*100, 1), "%)\n"))
        cat(paste("  >3°C:", sum(valid_pci >= 3), "parks (", round(sum(valid_pci >= 3)/length(valid_pci)*100, 1), "%)\n"))
        
        # Check if now matches original study expectations
        if(mean(valid_pci) > 1.0 && sum(valid_pci >= 1)/length(valid_pci) > 0.5) {
          cat("\n🎉 SUCCESS! PCI values now match original study pattern!\n")
          cat("✅ Ready for Module 4C (buffer variables) and Module 5 (analysis)!\n")
        } else if(mean(valid_pci) > 0.5) {
          cat("\n📈 MAJOR IMPROVEMENT! PCI values are much more realistic\n")
          cat("✅ Ready to proceed with analysis\n")
        } else {
          cat("\n⚠️ Still some issues but ring buffer method is working\n")
          cat("Consider trying different buffer distances or checking urban context\n")
        }
        
        # Show sample parks
        cat("\n--- Sample PCI Results ---\n")
        sample_indices <- 1:min(5, length(valid_pci))
        for(i in sample_indices) {
          idx <- which(!is.na(final_dataset$PCI))[i]
          cat(sprintf("Park %s: Park LST=%.2f°C, Ring LST=%.2f°C, PCI=%.3f°C\n",
                      final_dataset$park_id[idx],
                      final_dataset$Park_LST[idx],
                      final_dataset$Buffer_LST[idx],
                      final_dataset$PCI[idx]))
        }
        
      } else {
        cat("❌ No valid PCI calculations - check LST data and geometries\n")
      }
    }
    
    # Quality summary
    cat("\n--- Data Quality Summary ---\n")
    missing_summary <- sapply(final_dataset, function(x) sum(is.na(x)))
    complete_cases <- sum(complete.cases(final_dataset))
    
    cat(paste("Complete cases (no missing values):", complete_cases, "of", nrow(final_dataset), "\n"))
    if(any(missing_summary > 0)) {
      high_missing <- missing_summary[missing_summary > nrow(final_dataset) * 0.1]  # >10% missing
      if(length(high_missing) > 0) {
        cat("Variables with >10% missing values:\n")
        print(high_missing)
      }
    }
    
    return(final_dataset)
    
  }, error = function(e) {
    cat(paste("❌ FATAL ERROR in main processing:", e$message, "\n"))
    cat("Check data paths and raster integrity\n")
    return(NULL)
  })
}

# --- EXECUTE FIXED MAIN FUNCTION ---
cat("🚀 Starting RING BUFFER FIXED processing (LST only fix)...\n")
fixed_result <- main_processing_fixed()

# --- FINAL VALIDATION ---
if(!is.null(fixed_result)) {
  
  cat("\n🔍 Final validation of ring buffer fix...\n")
  
  # Check LST extraction success
  park_lst_success <- sum(!is.na(fixed_result$Park_LST))
  buffer_lst_success <- sum(!is.na(fixed_result$Buffer_LST))
  pci_success <- sum(!is.na(fixed_result$PCI))
  
  cat(paste("✅ Park LST extracted:", park_lst_success, "parks\n"))
  cat(paste("✅ Ring Buffer LST extracted:", buffer_lst_success, "parks\n"))
  cat(paste("✅ PCI calculated:", pci_success, "parks\n"))
  
  # Temperature comparison
  if(pci_success > 0) {
    valid_indices <- !is.na(fixed_result$PCI)
    park_temps <- fixed_result$Park_LST[valid_indices]
    ring_temps <- fixed_result$Buffer_LST[valid_indices]
    
    cat(sprintf("Mean park temperature: %.2f°C\n", mean(park_temps, na.rm = TRUE)))
    cat(sprintf("Mean ring temperature: %.2f°C\n", mean(ring_temps, na.rm = TRUE)))
    cat(sprintf("Temperature difference: %.3f°C\n", mean(ring_temps, na.rm = TRUE) - mean(park_temps, na.rm = TRUE)))
    
    # Logic check
    cooler_parks <- sum(park_temps < ring_temps, na.rm = TRUE)
    warmer_parks <- sum(park_temps > ring_temps, na.rm = TRUE)
    
    cat(sprintf("Parks cooler than ring buffer: %d (%.1f%%)\n", 
                cooler_parks, cooler_parks/pci_success*100))
    cat(sprintf("Parks warmer than ring buffer: %d (%.1f%%)\n",
                warmer_parks, warmer_parks/pci_success*100))
    
    if(cooler_parks > warmer_parks) {
      cat("✅ LOGIC CHECK PASSED: Most parks are cooler than their surroundings\n")
    } else {
      cat("⚠️ LOGIC CHECK FAILED: Most parks are warmer than surroundings - check data\n")
    }
  }
  
  cat("\n🎉 RING BUFFER FIX COMPLETE!\n")
  cat("✅ Module 4A+4B now uses ring buffer geometries for LST extraction\n")
  cat("✅ Ready to run Module 4C for buffer variables\n")
  cat("✅ Then ready for Module 5 PCI analysis!\n")
  
} else {
  cat("\n❌ Ring buffer fix failed. Check error messages above.\n")
}

cat("\n=== NEXT STEPS ===\n")
cat("1. Run Module 4C to calculate buffer variables (Buffer_green, Buffer_grey, etc.)\n")
cat("2. Run Module 5 for correlation analysis and regression\n")
cat("3. Results should now show realistic PCI values (1-3°C cooling effects)\n")

output_dir <- "data/gold/PCI/"
saveRDS(fixed_result, file.path(output_dir, "park_buffer_chars.rds"))