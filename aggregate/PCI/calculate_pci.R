#################################################################
# PCI CALCULATION: EXPLICIT RING BUFFER (DONUT SHAPE)
#################################################################

pacman::p_load(sf, terra, dplyr, exactextractr, ggplot2, tmap)

cat("🔧 CREATING EXPLICIT RING BUFFER (DONUT) FOR PCI\n")

# --- FUNCTION: Create and visualize ring buffer ---
create_ring_buffer_explicit <- function(park_geom, buffer_distance = 480) {
  
  # Step 1: Create outer buffer
  outer_buffer <- st_buffer(park_geom, dist = buffer_distance)
  
  # Step 2: Create ring by removing park area
  ring_buffer <- st_difference(outer_buffer, park_geom)
  
  # Step 3: Validate ring buffer
  park_area <- as.numeric(st_area(park_geom))
  outer_area <- as.numeric(st_area(outer_buffer))  
  ring_area <- as.numeric(st_area(ring_buffer))
  
  expected_ring_area <- outer_area - park_area
  
  cat(sprintf("  Park area: %.1f ha\n", park_area/10000))
  cat(sprintf("  Outer buffer area: %.1f ha\n", outer_area/10000))
  cat(sprintf("  Ring area: %.1f ha (expected %.1f ha)\n", 
              ring_area/10000, expected_ring_area/10000))
  
  if(abs(ring_area - expected_ring_area) > 1000) {  # 0.1 ha tolerance
    cat("  ⚠️ Ring area doesn't match expected - geometry issue!\n")
  } else {
    cat("  ✅ Ring geometry looks correct\n")
  }
  
  return(list(
    park = park_geom,
    outer_buffer = outer_buffer,
    ring_buffer = ring_buffer,
    areas = list(park = park_area, outer = outer_area, ring = ring_area)
  ))
}

# --- FUNCTION: Test ring buffer on sample parks ---
test_ring_buffer_method <- function(n_test_parks = 5) {
  
  cat("--- Testing Ring Buffer Method on Sample Parks ---\n")
  
  # Load data
  park_data <- readRDS("data/gold/PCI/park_characteristics_ready_for_pci.rds")
  lst_raster <- rast("data/silver/PCI/lst_raster.tif")
  geometries <- st_read("data/silver/PCI/park_buffer_geometries_tpm_m.gpkg", quiet = TRUE)
  
  if("geometry_type" %in% names(geometries)) {
    park_geometries <- geometries[geometries$geometry_type == "park", ]
  } else {
    park_geometries <- geometries
  }
  
  # Test first n parks
  test_results <- data.frame()
  
  for(i in 1:min(n_test_parks, nrow(park_data))) {
    park_id <- park_data$park_id[i]
    cat(sprintf("\n=== TESTING PARK %s ===\n", park_id))
    
    park_geom <- park_geometries[park_geometries$park_id == park_id, ]
    
    if(nrow(park_geom) > 0) {
      
      # Create ring buffer
      ring_result <- create_ring_buffer_explicit(park_geom, buffer_distance = 480)
      
      # Extract temperatures
      tryCatch({
        # Park temperature
        park_lst <- exact_extract(lst_raster, ring_result$park)[[1]]
        park_temp <- mean(park_lst$value, na.rm = TRUE)
        park_pixels <- sum(!is.na(park_lst$value))
        
        # Ring temperature (THIS IS THE KEY!)
        ring_lst <- exact_extract(lst_raster, ring_result$ring_buffer)[[1]]
        ring_temp <- mean(ring_lst$value, na.rm = TRUE)
        ring_pixels <- sum(!is.na(ring_lst$value))
        
        # Old method (full buffer including park)
        outer_lst <- exact_extract(lst_raster, ring_result$outer_buffer)[[1]]
        outer_temp <- mean(outer_lst$value, na.rm = TRUE)
        
        # Calculate PCI with different methods
        pci_ring <- ring_temp - park_temp        # NEW: Ring only
        pci_old <- outer_temp - park_temp        # OLD: Full buffer
        
        cat(sprintf("Temperatures:\n"))
        cat(sprintf("  Park interior: %.3f°C (%d pixels)\n", park_temp, park_pixels))
        cat(sprintf("  Ring buffer: %.3f°C (%d pixels)\n", ring_temp, ring_pixels))
        cat(sprintf("  Full buffer (old): %.3f°C\n", outer_temp))
        
        cat(sprintf("PCI Results:\n"))
        cat(sprintf("  Ring method: %.3f°C\n", pci_ring))
        cat(sprintf("  Old method: %.3f°C\n", pci_old))
        cat(sprintf("  Improvement: %.3f°C\n", pci_ring - pci_old))
        
        # Store results
        test_results <- rbind(test_results, data.frame(
          park_id = park_id,
          park_temp = park_temp,
          ring_temp = ring_temp,
          outer_temp = outer_temp,
          pci_ring = pci_ring,
          pci_old = pci_old,
          improvement = pci_ring - pci_old,
          park_pixels = park_pixels,
          ring_pixels = ring_pixels
        ))
        
      }, error = function(e) {
        cat(sprintf("Error extracting temperatures: %s\n", e$message))
      })
      
    } else {
      cat("No geometry found for this park\n")
    }
  }
  
  return(test_results)
}

# --- FUNCTION: Visualize ring buffer for one park ---
visualize_ring_buffer <- function(park_id_to_show) {
  
  cat(sprintf("--- Visualizing Ring Buffer for Park %s ---\n", park_id_to_show))
  
  # Load data
  lst_raster <- rast("data/silver/PCI/lst_raster.tif")
  geometries <- st_read("data/silver/PCI/park_buffer_geometries_tpm_m.gpkg", quiet = TRUE)
  
  if("geometry_type" %in% names(geometries)) {
    park_geometries <- geometries[geometries$geometry_type == "park", ]
  } else {
    park_geometries <- geometries
  }
  
  park_geom <- park_geometries[park_geometries$park_id == park_id_to_show, ]
  
  if(nrow(park_geom) > 0) {
    
    # Create ring
    ring_result <- create_ring_buffer_explicit(park_geom, buffer_distance = 480)
    
    # Create a small area around park for cropping LST
    plot_buffer <- st_buffer(park_geom, dist = 800)  # Slightly larger for context
    
    # Crop LST raster to plot area
    lst_crop <- crop(lst_raster, plot_buffer)
    lst_crop <- mask(lst_crop, plot_buffer)
    
    # Convert raster to dataframe for ggplot
    lst_df <- as.data.frame(lst_crop, xy = TRUE)
    names(lst_df)[3] <- "LST"
    
    # Create plot
    p <- ggplot() +
      # LST background
      geom_raster(data = lst_df, aes(x = x, y = y, fill = LST)) +
      scale_fill_viridis_c(name = "LST (°C)") +
      # Park boundary
      geom_sf(data = ring_result$park, fill = "red", alpha = 0.3, color = "red", size = 2) +
      # Ring buffer
      geom_sf(data = ring_result$ring_buffer, fill = "blue", alpha = 0.2, color = "blue", size = 1) +
      # Outer buffer outline
      geom_sf(data = ring_result$outer_buffer, fill = NA, color = "black", size = 1, linetype = "dashed") +
      labs(title = sprintf("Ring Buffer for Park %s", park_id_to_show),
           subtitle = "Red = Park, Blue = Ring Buffer (PCI measurement area)") +
      theme_minimal() +
      theme(axis.text = element_text(size = 8))
    
    # Save plot
    ggsave(sprintf("outputs/ring_buffer_park_%s.png", park_id_to_show), 
           p, width = 10, height = 8, dpi = 300)
    
    cat(sprintf("Plot saved as: outputs/ring_buffer_park_%s.png\n", park_id_to_show))
    
    return(p)
  } else {
    cat("Park not found!\n")
    return(NULL)
  }
}

# --- EXECUTE TESTS ---
dir.create("outputs", showWarnings = FALSE)

cat("🚀 Testing ring buffer method...\n")
test_results <- test_ring_buffer_method(n_test_parks = 8)

# --- ANALYZE TEST RESULTS ---
if(nrow(test_results) > 0) {
  cat("\n=== RING BUFFER TEST RESULTS ===\n")
  
  cat(sprintf("Average improvements:\n"))
  cat(sprintf("  Ring method PCI: %.3f°C (vs old %.3f°C)\n", 
              mean(test_results$pci_ring, na.rm = TRUE),
              mean(test_results$pci_old, na.rm = TRUE)))
  cat(sprintf("  Average improvement: %.3f°C\n", 
              mean(test_results$improvement, na.rm = TRUE)))
  
  # Show individual results
  cat("\nIndividual park results:\n")
  for(i in 1:nrow(test_results)) {
    cat(sprintf("Park %s: Ring PCI = %.3f°C (vs old %.3f°C) [+%.3f°C]\n",
                test_results$park_id[i],
                test_results$pci_ring[i], 
                test_results$pci_old[i],
                test_results$improvement[i]))
  }
  
  # Visualize one park
  if(nrow(test_results) > 0) {
    best_park <- test_results$park_id[which.max(test_results$pci_ring)]
    cat(sprintf("\nCreating visualization for best park: %s\n", best_park))
    viz_plot <- visualize_ring_buffer(best_park)
  }
  
  # Check if results are now reasonable
  if(mean(test_results$pci_ring, na.rm = TRUE) > 1.0) {
    cat("\n🎉 SUCCESS! Ring buffer method gives realistic PCI values!\n")
    cat("Ready to apply to all parks.\n")
  } else {
    cat("\n📈 IMPROVEMENT but still low. May need:\n")
    cat("- Different buffer distance\n")
    cat("- Check if parks are in urban areas\n") 
    cat("- Verify LST data quality\n")
  }
  
} else {
  cat("❌ No successful test results\n")
}

cat("\n✅ Ring buffer test complete!\n")
cat("Check the visualization to see exactly what area we're measuring.\n")