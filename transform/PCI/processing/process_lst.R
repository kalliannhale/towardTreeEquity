# ===================================================================
#
#   Module: process_lst.R
#   Author: Kalli A. Hale
#   Date: Tuesday, August 5th, 2025
#   Description: A module to process raw Landsat 8 land surface
#                temperature (LST) data. It loads a raw raster file
#                (e.g., representing a temporal mean or median),
#                converts it to a standard temperature scale, and
#                clips it to a given boundary.
#
# ===================================================================

# --- Dependencies ---
pacman::p_load(httr2, terra, sf)


# --- Constants ---
# These are specific to Landsat 8 Collection 2, Level 2 thermal data.
# They are independent of whether the input is a mean, median, etc.
LANDSAT8_SCALE <- 0.00341802
LANDSAT8_OFFSET <- 149.0


# ===================================================================
#   Main Processing Function
# ===================================================================

#' Process Raw Land Surface Temperature (LST) Raster
#'
#' This function takes a raw Landsat 8 LST raster file, converts the pixel
#' values to a standard temperature unit, and clips/masks the result to a
#' specified boundary shapefile.
#'
#' @param raster_path A string path to the input raw LST .tif file.
#' @param boundary_path A string path to the boundary .shp file.
#' @param aggregation_type A string describing the temporal aggregation of the
#'                         input raster (e.g., "Median", "Mean"). This is used
#'                         for naming the output layer. Defaults to "Median".
#' @param target_unit A string indicating the desired output unit.
#'                    Can be "Celsius" (default) or "Fahrenheit".
#'
#' @return A SpatRaster object from the 'terra' package containing the
#'         processed, clipped, and masked LST data. The raster layer will be
#'         named based on the aggregation_type and target_unit.
#'
#' @examples
#' \dontrun{
#'   # Assuming this script is in your working directory
#'   source("process_lst.R")
#'
#'   # Define file paths for a MEDIAN temperature file
#'   median_tif_file <- "data/input/surfacetemperature_median_2020_2022.tif"
#'   shp_file <- "data/input/nyc_boroughs/nybb.shp"
#'
#'   # Get Celsius (default)
#'   nyc_lst_celsius <- process_lst(
#'     raster_path = median_tif_file,
#'     boundary_path = shp_file
#'   )
#'
#'   # Get Fahrenheit
#'   nyc_lst_fahrenheit <- process_lst(
#'     raster_path = median_tif_file,
#'     boundary_path = shp_file,
#'     target_unit = "Fahrenheit"
#'   )
#' }
scale_lst <- function(raster_path,
                        boundary_path,
                        aggregation_type = "Median",
                        target_unit = "Celsius") {
  
  # --- 1. Input Validation ---
  if (!file.exists(raster_path)) {
    stop("Raster file not found at: ", raster_path)
  }
  if (!file.exists(boundary_path)) {
    stop("Boundary shapefile not found at: ", boundary_path)
  }
  if (!target_unit %in% c("Fahrenheit", "Celsius")) {
    stop("`target_unit` must be either 'Fahrenheit' or 'Celsius'.")
  }
  
  # --- 2. Load Data ---
  lst_raw <- terra::rast(raster_path)
  boundary_sf <- sf::st_read(boundary_path, quiet = TRUE)
  
  # --- 3. Convert Temperature ---
  # Apply scale and offset to get temperature in Kelvin
  lst_kelvin <- lst_raw * LANDSAT8_SCALE + LANDSAT8_OFFSET
  
  # Convert from Kelvin to Celsius (base for both conversions)
  lst_celsius <- lst_kelvin - 273.15
  
  # Convert to the target unit if necessary
  if (target_unit == "Fahrenheit") {
    lst_final_temp <- lst_celsius * (9 / 5) + 32
  } else { # Celsius
    lst_final_temp <- lst_celsius
  }
  
  return(lst_final_temp)
}