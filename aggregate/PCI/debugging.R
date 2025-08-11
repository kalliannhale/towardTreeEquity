# Add this code after you've run your main function and have 'fixed_result'

pacman::p_load(sf, terra, dplyr, exactextractr, landscapemetrics, purrr, parallel, data.table)

# --- VISUALIZATION DEBUGGING ---

# 1. Load the necessary data
lst_raster <- rast("data/silver/PCI/lst_raster.tif")
all_geometries <- sf::st_read("data/silver/PCI/park_buffer_geometries_tpm_m.gpkg")

# 2. Find a park with a negative PCI from your results
problem_parks <- fixed_result[fixed_result$PCI < 0 & !is.na(fixed_result$PCI), ]

if (nrow(problem_parks) > 0) {
  
  # Pick the park with the MOST negative PCI to see the issue clearly
  problem_park_id <- problem_parks$park_id[which.min(problem_parks$PCI)]
  
  cat(paste("\n--- Debugging Park ID:", problem_park_id, "---\n"))
  
  # 3. Get the specific geometries for this park
  park_geom <- all_geometries %>% filter(park_id == problem_park_id, geometry_type == "park")
  ring_geom <- all_geometries %>% filter(park_id == problem_park_id, geometry_type == "buffer_ring", buffer_distance == fixed_result$buffer_distance_used[1])
  
  # 4. Create the plot
  debug_plot <- ggplot() +
    # Plot the LST raster in the background, cropped to the area of interest
    geom_spatraster(data = lst_raster) +
    
    # Add the park polygon
    geom_sf(data = park_geom, fill = "green", color = "darkgreen", alpha = 0.5) +
    debug
  # Add the ring buffer polygon
  geom_sf(data = ring_geom, fill = NA, color = "magenta", linewidth = 1.2) +
    
    # Use a good color scale for temperature
    scale_fill_viridis_c(name = "LST (°C)", na.value = "transparent") +
    
    labs(
      title = paste("Visual Inspection of Park ID:", problem_park_id),
      subtitle = sprintf("PCI: %.2f°C (Park LST: %.2f°C, Ring LST: %.2f°C)", 
                         problem_parks$PCI[which.min(problem_parks$PCI)],
                         problem_parks$Park_LST[which.min(problem_parks$PCI)],
                         problem_parks$Buffer_LST[which.min(problem_parks$PCI)]),
      caption = "Park is green, Ring Buffer is magenta"
    ) +
    theme_minimal() +
    # Zoom in on the area of the ring
    coord_sf(
      xlim = st_bbox(ring_geom)$xlim,
      ylim = st_bbox(ring_geom)$ylim,
      expand = TRUE
    )
  
  print(debug_plot)
  
} else {
  cat("\n--- No parks with negative PCI found for debugging. That's great! ---\n")
}