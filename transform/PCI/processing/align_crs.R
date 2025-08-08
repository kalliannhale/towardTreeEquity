#################################################################
# BEAT 3: DEFINING PARK & BUFFER GEOMETRIES (TPM-M Method)
# Goal: Create park geometries and multiple buffer rings at 30m intervals
#################################################################

cat("--- Module 3: Creating TPM-M buffer geometries ---\n")

# --- 3.1: Load Processed Data from Module 2 ---
cat("--- Loading processed data from Module 2 ---\n")

parks <- st_read("data/silver/PCI/parks.gpkg")
cat(paste("Loaded", nrow(parks), "parks\n"))

# --- 3.2: Set up TPM-M Parameters ---
buffer_distances <- seq(30, 600, by = 30)  
cat(paste("Buffer distances:", paste(buffer_distances, collapse = ", "), "\n"))

# Function to create geometries for one park
create_park_geometries <- function(park_row, buffer_distances) {
  park_id <- park_row[["objectid"]]
  park_geom <- st_geometry(park_row)
  
  geometry_list <- list()
  
  # Add the park itself
  park_df <- data.frame(
    park_id = park_id,
    geometry_type = "park", 
    buffer_distance = 0
  )
  geometry_list[[1]] <- st_sf(park_df, geometry = park_geom)
  
  # Create buffer rings
  previous_buffer <- park_geom  
  
  for(i in seq_along(buffer_distances)) {
    distance <- buffer_distances[i]
    current_buffer <- st_buffer(park_geom, dist = distance)
    buffer_ring <- st_difference(current_buffer, previous_buffer)
    
    ring_df <- data.frame(
      park_id = park_id,
      geometry_type = "buffer_ring",
      buffer_distance = distance
    )
    geometry_list[[i + 1]] <- st_sf(ring_df, geometry = buffer_ring)
    previous_buffer <- current_buffer
  }
  
  return(do.call(rbind, geometry_list))
}

# --- 3.3: Process All Parks with Large Batches ---
batch_size <- 250  # 250 parks per batch
total_parks <- nrow(parks)
num_batches <- ceiling(total_parks / batch_size)
output_dir <- "data/silver/PCI/"

cat(paste("Total parks:", total_parks, "\n"))
cat(paste("Batch size:", batch_size, "\n"))
cat(paste("Number of batches:", num_batches, "\n"))

# Check what batches already exist (for resuming)
existing_batches <- list.files(output_dir, pattern = "park_geometries_batch_.*\\.gpkg$")
completed_batches <- as.numeric(gsub("park_geometries_batch_(\\d+)\\.gpkg", "\\1", existing_batches))

cat(paste("Found", length(completed_batches), "existing batch files\n"))

# Process each batch
for(batch_num in 1:num_batches) {
  
  # Skip if this batch already exists
  if(batch_num %in% completed_batches) {
    cat(paste("Skipping batch", batch_num, "(already completed)\n"))
    next
  }
  
  # Calculate park indices for this batch
  start_idx <- (batch_num - 1) * batch_size + 1
  end_idx <- min(batch_num * batch_size, total_parks)
  park_indices <- start_idx:end_idx
  
  cat(paste("Processing batch", batch_num, "of", num_batches, 
            "- parks", start_idx, "to", end_idx, "(", length(park_indices), "parks )\n"))
  
  # Process this batch
  batch_start_time <- Sys.time()
  
  batch_geometries <- map_dfr(park_indices, function(i) {
    if(i %% 50 == 0) cat(paste("  -> Park", i, "\n"))
    create_park_geometries(parks[i, ], buffer_distances)
  })
  
  batch_end_time <- Sys.time()
  batch_duration <- as.numeric(batch_end_time - batch_start_time, units = "mins")
  
  # Save this batch
  batch_filename <- file.path(output_dir, paste0("park_geometries_batch_", batch_num, ".gpkg"))
  st_write(batch_geometries, batch_filename, delete_dsn = TRUE)
  
  cat(paste("  -> Saved batch", batch_num, ":", nrow(batch_geometries), "geometries"))
  cat(paste(" (", round(batch_duration, 2), "minutes )\n"))
  
  # Estimate time remaining after first batch
  if(batch_num == 1) {
    estimated_total_time <- batch_duration * num_batches
    cat(paste("  -> Estimated total time:", round(estimated_total_time, 1), "minutes\n"))
  }
}

cat("--- All batches processed! Now combining... ---\n")

# Combine all batch files
batch_files <- list.files(output_dir, pattern = "park_geometries_batch_.*\\.gpkg$", full.names = TRUE)
batch_files <- sort(batch_files)  # Ensure proper order

cat(paste("Combining", length(batch_files), "batch files\n"))

all_geometries_list <- map(batch_files, st_read)
park_buffer_geometries <- do.call(rbind, all_geometries_list)

# Save final combined result
st_write(park_buffer_geometries, file.path(output_dir, "park_buffer_geometries_tpm_m.gpkg"), delete_dsn = TRUE)

cat("--- Module 3 Complete! ---\n")
cat(paste("Final result:", nrow(park_buffer_geometries), "total geometries\n"))
cat(paste("Expected:", total_parks * 21, "geometries (park + 20 buffer rings)\n"))

# Summary statistics
cat("\nSummary:\n")
print(table(park_buffer_geometries$geometry_type))
cat(paste("Unique parks:", length(unique(park_buffer_geometries$park_id)), "\n"))