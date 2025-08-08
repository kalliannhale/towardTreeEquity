#################################################################
### BEAT 1: SETUP & DATA LOADING (Concise)
#################################################################

pacman::p_load(httr2, terra, sf, dplyr, yaml, purrr, landscapemetrics, exactextractr)

# --- Load Data Paths from YAML Manifest ---
manifest <- yaml::read_yaml("config/ingredients/PCI/nyc_pci_sources.yml")

# --- 1.3: Load All Raw Data Sources ---
# Land Surface Temperature
lst_raster_raw <- terra::rast(manifest$sources$land_surface_temp$location)

# NDVI
ndvi_raster_raw <- terra::rast(manifest$sources$ndvi$location)

# Park Geometries
parks_path <- file.path(
  manifest$sources$park_geometries$location,
  manifest$sources$park_geometries$processing$validation_target
)
parks_raw <- sf::st_read(parks_path)

# Land Cover
land_cover_path <- file.path(
  manifest$sources$land_cover$location,
  manifest$sources$land_cover$processing$validation_target
)
land_cover_raster_raw <- terra::rast(land_cover_path)

# Road Network
roads_path <- file.path(
  manifest$sources$road_network$location,
  manifest$sources$road_network$processing$validation_target
)
# Note: This will auto-select the first layer. We will correct this in Beat 2.
roads_raw <- sf::st_read(roads_path)

# City Boundaries
city_boundaries_path <- file.path(
  manifest$sources$city_boundaries$location,
  manifest$sources$city_boundaries$processing$validation_target
)
city_boundary_raw <- sf::st_read(city_boundaries_path)
