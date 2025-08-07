# File: main_analysis.R

# --- 1. Setup ---
pacman::p_load(httr2, terra, sf, ggplot2, tidyterra, here)


# Source our updated, more flexible module
source("ingest/urban parks/process_lst.R")

# --- 2. Define File Paths ---
# Use the path to your MEDIAN temperature file
raw_median_temp_file <- "data/bronze/surfacetemperature_median_2020_2022.tif"
nyc_boundary_file <- "data/bronze/nybb_25b/nybb.shp"

# --- 3. Process the LST Data ---
# Call the function, explicitly noting the aggregation type
nyc_lst_median_f <- process_lst(
  raster_path = raw_median_temp_file,
  boundary_path = nyc_boundary_file,
  aggregation_type = "Median", # Good practice to be explicit
  target_unit = "Fahrenheit"
)

# --- 4. Verification ---
# Check the output. Note the descriptive layer name!
print(nyc_lst_median_f)
#> class       : SpatRaster
#> ...
#> names       : LST_Median_Fahrenheit
#> min value   : ...
#> max value   : ...

# --- 5. Further Analysis ---
# Your subsequent analysis logic remains the same. Remember to use the spatial
# median when summarizing temperatures within parks and buffers for robustness!
#
# median_park_temps <- terra::extract(nyc_lst_median_f, parks_sf, fun = "median", ...)