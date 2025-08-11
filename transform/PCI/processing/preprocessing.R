#################################################################
### BEAT 1: SETUP & DATA LOADING
### Goal: Load all libraries and raw data sources.
#################################################################

# --- 1.1: Install and Load Libraries ---
# This code checks if packages are installed and installs them if not.

pacman::p_load(httr2, terra, sf, dplyr, yaml, purrr, landscapemetrics, exactextractr)

source("transform/PCI/processing/process_lst.R")

# --- Load Data Paths from YAML Manifest ---
manifest <- yaml::read_yaml("config/ingredients/PCI/nyc_pci_sources.yml")

cat("--- Project manifest 'nyc_pci_sources.yml' read successfully. ---\n\n")

# --- 1.3: Load All Raw Data Sources ---
# We will now explicitly build the full path for any source that has a 'validation_target'.

cat("--- Loading data sources specified in manifest... ---\n")

# Load Land Surface Temperature (LST) Raster (no validation_target needed)
lst_path <- manifest$sources$land_surface_temp$location
lst_raster_raw <- terra::rast(lst_path)
cat(" -> Loaded: Land Surface Temperature\n")

# Load NDVI Raster (no validation_target needed)
ndvi_path <- manifest$sources$ndvi$location
ndvi_raster_raw <- terra::rast(ndvi_path)
cat(" -> Loaded: NDVI\n")

# Load Park Geometries (Vector Shapefile)
# We build the full path using the location and the validation_target.
parks_path <- file.path(
  manifest$sources$park_geometries$location,
  manifest$sources$park_geometries$processing$validation_target
)
parks_raw <- sf::st_read(parks_path)
cat(" -> Loaded: Park Geometries\n")

# Load Land Cover Raster (Erdas Imagine .img)
land_cover_path <- file.path(
  manifest$sources$land_cover$location,
  manifest$sources$land_cover$processing$validation_target
)
land_cover_raster_raw <- terra::rast(land_cover_path)
cat(" -> Loaded: Land Cover\n")

# Load Nighttime Lights Raster (no validation_target needed)
ntl_path <- manifest$sources$night_time_light$location
ntl_raster_raw <- terra::rast(ntl_path)
cat(" -> Loaded: Nighttime Lights\n")

# Load Road Network (Vector File Geodatabase)
# This was also correct, building the path to the .gdb.
roads_path <- file.path(
  manifest$sources$road_network$location,
  manifest$sources$road_network$processing$validation_target
)
roads_raw <- sf::st_read(roads_path)
cat(" -> Loaded: Road Network\n")

# Load City Boundaries (Vector Shapefile) - CORRECTED
# Applying the same fix here for consistency and correctness.
city_boundaries_path <- file.path(
  manifest$sources$city_boundaries$location,
  manifest$sources$city_boundaries$processing$validation_target
)
city_boundary_raw <- sf::st_read(city_boundaries_path)
cat(" -> Loaded: City Boundaries\n")

cat("\n--- All raw data has been loaded into R. ---\n")


# --- 1.4: Verification Step ---
# print a summary of each loaded object to make sure they look right.

cat("\n--- Verifying loaded data objects: ---\n")
print("LST Raster:")
print(lst_raster_raw)

print("--------------------")
print("Park Geometries:")
print(parks_raw)

print("--------------------")
print("City Boundaries:")
print(city_boundary_raw)

#################################################################
# BEAT 2: CRS, Unit Conversion, & Data Alignment
#################################################################

# --- 2.1: Define Target CRS ---
target_crs_string <- "EPSG:2263" # NAD83 / New York Long Island (ftUS)
cat("--- Target CRS for the project defined as:", target_crs_string, "---\n\n")


# --- 2.2: Process Each Raw Data Source into an Analysis-Ready Object ---
cat("--- Processing raw data layers... ---\n")

# --- LST Raster (Source: WGS84 Degrees) ---

lst_raster_raw <- scale_lst(lst_path, city_boundaries_path, "Median", "Celsius")

# Create a cropping boundary IN THE SAME CRS as the LST raster (WGS84).
cropping_extent_wgs84 <- sf::st_transform(parks_raw, crs = st_crs(lst_raster_raw))
lst_raster_cropped <- terra::crop(lst_raster_raw, cropping_extent_wgs84)
lst_raster_proj <- terra::project(lst_raster_cropped, target_crs_string)
cat(" -> Processed: LST Raster\n")

# --- NDVI Raster (Source: WGS84 Degrees) ---
# We can reuse the same WGS84 cropping boundary.
ndvi_raster_cropped <- terra::crop(ndvi_raster_raw, cropping_extent_wgs84)
ndvi_raster <- terra::project(ndvi_raster_cropped, target_crs_string)
cat(" -> Processed: NDVI Raster\n")

# --- Nighttime Lights Raster (Source: Assumed WGS84 from Google Earth Engine) ---
# Check CRS and handle accordingly
cat("NTL Raster CRS:", st_crs(ntl_raster_raw)$wkt, "\n")

# If NTL is in WGS84 (common for GEE exports), use the WGS84 boundary
if (st_crs(ntl_raster_raw) == st_crs("EPSG:4326")) {
  ntl_raster_cropped <- terra::crop(ntl_raster_raw, cropping_extent_wgs84)
  ntl_raster <- terra::project(ntl_raster_cropped, target_crs_string)
  cat(" -> Processed: NTL Raster (from WGS84)\n")
} else {
  # If it's already in target CRS or another CRS, handle accordingly
  cropping_extent_ntl <- sf::st_transform(parks_raw, crs = st_crs(ntl_raster_raw))
  ntl_raster_cropped <- terra::crop(ntl_raster_raw, cropping_extent_ntl)
  
  # Check if projection is needed
  if (st_crs(ntl_raster_raw) != st_crs(target_crs_string)) {
    ntl_raster <- terra::project(ntl_raster_cropped, target_crs_string)
    cat(" -> Processed: NTL Raster (reprojected)\n")
  } else {
    ntl_raster <- ntl_raster_cropped
    cat(" -> Processed: NTL Raster (no reprojection needed)\n")
  }
}

# --- Land Cover Raster (Source: NAD83 Feet) ---
# Create a cropping boundary IN THE SAME CRS as the Land Cover raster (NAD83).
cropping_extent_nad83 <- sf::st_transform(parks_raw, crs = st_crs(land_cover_raster_raw))
# Now the crop will work because the extents overlap.
land_cover_raster_cropped <- terra::crop(land_cover_raster_raw, cropping_extent_nad83)
# Since it's already in the target CRS, we just assign it. No projection needed.
land_cover_raster <- land_cover_raster_cropped
cat(" -> Processed: Land Cover Raster\n")

# --- 2.3: Process Vector Data ---
cat("\n--- Processing raw vector layers... ---\n")
# Parks and City Boundary
parks <- sf::st_transform(parks_raw, crs = target_crs_string)
city_boundary <- city_boundary_raw
cat(" -> Processed: Park Geometries and City Boundaries\n")

# --- Roads Vector (The Full, Corrected Workflow) ---
# Step A: Load the raw mixed-geometry layer from the geodatabase.
roads_raw_mixed <- sf::st_read(roads_path, layer = "lion")

# Step B: Filter to keep ONLY linestring features.
roads_lines_only <- roads_raw_mixed %>%
  filter(st_is(st_geometry(.), c("LINESTRING", "MULTILINESTRING")))
cat(paste(" -> Filtered road network: kept", nrow(roads_lines_only), "line features.\n"))

# Step C: Reproject the line-only data to our target CRS.
roads_proj <- sf::st_transform(roads_lines_only, crs = target_crs_string)

# Step D: Repair any invalid line geometries.
roads <- sf::st_make_valid(roads_proj)
cat(" -> Repaired and processed: Road Network\n")

# --- 2.4: Final Unified Crop ---
cat("\n--- Cropping all layers to a final, unified study area... ---\n")
final_boundary <- st_union(city_boundary)
padded_boundary <- st_buffer(final_boundary, dist = 1000)

lst_raster <- terra::crop(lst_raster_proj, padded_boundary, snap="out")
ndvi_raster <- terra::crop(ndvi_raster, padded_boundary, snap="out")
ntl_raster <- terra::crop(ntl_raster, padded_boundary, snap="out")
land_cover_raster <- terra::crop(land_cover_raster, padded_boundary, snap="out")
parks <- sf::st_filter(parks, padded_boundary)
roads <- sf::st_filter(roads, padded_boundary)

cat("\n--- All layers have been aligned and cropped. ---\n")
cat("--- Beat 2 is complete. ---\n")

# Save raster objects using terra::writeRaster()
terra::writeRaster(land_cover_raster, "data/silver/PCI/land_cover_raster.tif", overwrite=TRUE)
terra::writeRaster(lst_raster, "data/silver/PCI/lst_raster.tif", overwrite=TRUE) 
terra::writeRaster(ndvi_raster, "data/silver/PCI/ndvi_raster.tif", overwrite=TRUE)
terra::writeRaster(ntl_raster, "data/silver/PCI/ntl_raster.tif", overwrite=TRUE)

# Save vector objects using sf::st_write()
sf::st_write(parks, "data/silver/PCI/parks.gpkg", delete_dsn=TRUE)
sf::st_write(roads, "data/silver/PCI/roads.gpkg", delete_dsn=TRUE)
sf::st_write(city_boundary, "data/silver/PCI/city_boundary.gpkg", delete_dsn=TRUE)

cat("--- All processed data saved to disk ---\n")