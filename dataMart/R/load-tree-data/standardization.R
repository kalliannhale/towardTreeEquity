"
Kalli A. Hale
[if you're a contributor, your name goes here :)]
___

June 2025 
___ 

This module loads and standardizes municipal tree 
  canopy data from various sources; handling multiple 
  file formats and performing basic validation.

Takes in a file path, API endpoint, or database connection.
  - specify file format:
      (+) 'csv', 'shp', 'geojson', or 'gpkg'
"

pacman::p_load(tidyr, dplyr)

match_fields <- function(source_fields, canonical_fields) {
  # Use string distance, common abbreviations, semantic similarity
  # "PID" scores high match with "parcel_id"
  # "LU" scores high match with "land_use" or "land_use_type"
}

