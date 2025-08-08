# data_ingestion_pipeline.R
# 
# Enhanced data ingestion pipeline for Medallion architecture
# Processes manifest files and uploads data sources to Supabase
# 
# Author: Enhanced version for NYC PCI project
# Dependencies: yaml, httr2, fs, zip, dplyr, purrr, glue

pacman::p_load(yaml, httr2, fs, zip, dplyr, purrr, glue)

# Source the storage helpers
source("ingest/utils/sb_storage_helpers.R")

#' Load and parse project manifest YAML file
#' @param manifest_path Path to the manifest YAML file
#' @return List containing manifest data
load_manifest <- function(manifest_path) {
  if (!file.exists(manifest_path)) {
    stop("Manifest file not found: ", manifest_path)
  }
  yaml::read_yaml(manifest_path)
}

#' Handle local file sources
#' @param source_config Configuration for a single source
#' @return Path to file ready for upload
handle_local_source <- function(source_config) {
  location <- source_config$location
  
  # Check if source needs special processing
  if ("processing" %in% names(source_config)) {
    return(process_with_handler(source_config))
  }
  
  # For single files, return the path directly
  if (file.exists(location) && !dir.exists(location)) {
    return(location)
  } else {
    stop("Local file not found: ", location)
  }
}

#' Handle URL-based sources by downloading them locally first
#' @param source_config Configuration for a single source  
#' @return Path to downloaded file
handle_url_source <- function(source_config) {
  url <- source_config$location
  temp_dir <- "temp_downloads"
  
  # Create temp directory if it doesn't exist
  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, recursive = TRUE)
  }
  
  # Generate filename from source_id or URL
  filename <- source_config$source_id %||% basename(url)
  local_path <- file.path(temp_dir, filename)
  
  # Download file
  tryCatch({
    req <- request(url)
    resp <- req_perform(req, path = local_path)
    
    if (resp_status(resp) == 200) {
      message("✅ Downloaded: ", filename)
      return(local_path)
    } else {
      stop("Download failed with status: ", resp_status(resp))
    }
  }, error = function(e) {
    stop("Failed to download from URL: ", url, "\nError: ", e$message)
  })
}

#' Handle API-based sources (placeholder for future implementation)
#' @param source_config Configuration for a single source
#' @return Path to processed file
handle_api_source <- function(source_config) {
  stop("API source handling not yet implemented")
}

#' Process sources that need special handlers
#' @param source_config Configuration for a single source
#' @return Path to processed file
process_with_handler <- function(source_config) {
  location <- source_config$location
  handler <- source_config$processing$handler
  validation_target <- source_config$processing$validation_target
  
  if (handler == "zip_folder") {
    return(handle_zip_folder(location, validation_target, source_config))
  } else {
    stop("Unknown handler: ", handler)
  }
}

#' Zip a folder and validate it contains the expected target file (Enhanced)
#' @param folder_path Path to folder to zip
#' @param validation_target Target file to validate exists
#' @param source_config Source configuration
#' @return Path to created zip file
handle_zip_folder <- function(folder_path, validation_target, source_config) {
  message("📁 Processing folder: ", folder_path)
  
  if (!dir.exists(folder_path)) {
    stop("Directory not found: ", folder_path)
  }
  
  # Validate the target file exists
  target_path <- file.path(folder_path, validation_target)
  if (!file.exists(target_path)) {
    stop("Validation target not found: ", target_path)
  }
  message("✅ Validation target found: ", validation_target)
  
  # Get folder size info
  all_files <- list.files(folder_path, recursive = TRUE, full.names = TRUE)
  total_size_mb <- sum(file.size(all_files), na.rm = TRUE) / (1024^2)
  message("📊 Folder contains ", length(all_files), " files, total size: ", round(total_size_mb, 1), " MB")
  
  # Create zip file
  source_id <- source_config$source_id %||% "data"
  zip_filename <- paste0("temp_", source_id, ".zip")
  
  message("🗜️  Creating zip file: ", zip_filename)
  message("   This may take a while for large folders...")
  
  tryCatch({
    # Use a more robust zipping approach
    old_wd <- getwd()
    setwd(dirname(folder_path))
    folder_name <- basename(folder_path)
    
    # Create zip with faster compression
    start_time <- Sys.time()
    zip_result <- zip::zip(
      zipfile = file.path(old_wd, zip_filename),
      files = list.files(folder_name, full.names = TRUE, recursive = TRUE),
      compression_level = 1  # Faster compression
    )
    
    end_time <- Sys.time()
    zip_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    message("⏱️  Zip completed in ", round(zip_time, 1), " seconds")
    
    setwd(old_wd)
    
    # Check if zip was created successfully
    if (file.exists(zip_filename)) {
      zip_size_mb <- file.size(zip_filename) / (1024^2)
      message("✅ Zip file created: ", round(zip_size_mb, 1), " MB")
      return(zip_filename)
    } else {
      stop("Zip file was not created successfully")
    }
    
  }, error = function(e) {
    setwd(old_wd)  # Make sure we reset working directory
    stop("Failed to create zip file: ", e$message)
  })
}

#' Enhanced upload function with filename sanitization and progress
#' @param file_path Path to local file
#' @param source_config Source configuration
#' @return TRUE if successful, FALSE otherwise
upload_to_supabase <- function(file_path, source_config) {
  tryCatch({
    # Generate storage path with sanitized filename
    layer_name <- source_config$layer_name
    source_id <- source_config$source_id %||% "unknown"
    
    # Sanitize source_id (replace problematic characters)
    source_id <- gsub("[^a-zA-Z0-9_]", "_", source_id)
    
    file_extension <- paste0(".", tools::file_ext(file_path))
    if (file_extension == ".") file_extension <- ".zip"
    
    storage_path <- glue("{layer_name}/{source_id}{file_extension}")
    
    # Check file size
    file_size_mb <- file.size(file_path) / (1024^2)
    message("📁 File size: ", round(file_size_mb, 1), " MB")
    
    if (file_size_mb > 50) {
      message("⚠️  Large file detected - upload may take several minutes...")
    }
    
    message("📤 Uploading to: ", storage_path)
    
    # Use existing supa_upload function
    supa_upload(file_path, storage_path)
    
    return(TRUE)
    
  }, error = function(e) {
    message("❌ Error uploading ", basename(file_path), ": ", e$message)
    return(FALSE)
  })
}

#' Enhanced process_source function with better progress reporting
#' @param source_name Name of the source
#' @param source_config Configuration for the source
#' @return List with success status and any temporary files created
process_source <- function(source_name, source_config) {
  message("\n🔄 Processing source: ", source_name)
  message("   Type: ", source_config$source_type)
  message("   Location: ", source_config$location)
  
  temp_files <- character(0)
  
  tryCatch({
    # Handle different source types
    source_type <- source_config$source_type
    
    file_path <- switch(source_type,
                        "local" = handle_local_source(source_config),
                        "url" = {
                          path <- handle_url_source(source_config)
                          temp_files <<- c(temp_files, path)
                          path
                        },
                        "api" = {
                          path <- handle_api_source(source_config) 
                          temp_files <<- c(temp_files, path)
                          path
                        },
                        stop("Unknown source_type: ", source_type)
    )
    
    # Track temp files created by processing
    if (grepl("^temp_", basename(file_path))) {
      temp_files <- c(temp_files, file_path)
    }
    
    message("📤 Ready to upload: ", basename(file_path))
    
    # Upload to Supabase
    success <- upload_to_supabase(file_path, source_config)
    
    if (success) {
      message("✅ Successfully processed: ", source_name)
    }
    
    return(list(success = success, temp_files = temp_files))
    
  }, error = function(e) {
    message("❌ Failed to process ", source_name, ": ", e$message)
    return(list(success = FALSE, temp_files = temp_files))
  })
}

#' Clean up temporary files and directories
#' @param temp_files Vector of temporary file paths
cleanup_temp_files <- function(temp_files) {
  if (length(temp_files) == 0) return()
  
  message("\n🧹 Cleaning up temporary files...")
  
  # Remove temporary files
  for (file_path in temp_files) {
    if (file.exists(file_path)) {
      tryCatch({
        file.remove(file_path)
        message("   Removed: ", basename(file_path))
      }, error = function(e) {
        message("   ⚠️  Could not remove ", basename(file_path), ": ", e$message)
      })
    }
  }
  
  # Clean up temp directories
  temp_dirs <- c("temp_downloads")
  for (temp_dir in temp_dirs) {
    if (dir.exists(temp_dir)) {
      tryCatch({
        unlink(temp_dir, recursive = TRUE)
        message("   Removed directory: ", temp_dir)
      }, error = function(e) {
        message("   ⚠️  Could not remove directory ", temp_dir, ": ", e$message)
      })
    }
  }
}

#' Main function to process entire manifest and upload all sources
#' @param manifest_path Path to the manifest YAML file
process_manifest <- function(manifest_path) {
  message("🚀 Starting data ingestion pipeline...")
  message("⏰ Start time: ", Sys.time())
  
  # Load manifest
  manifest <- load_manifest(manifest_path)
  project_name <- manifest$project_name
  sources <- manifest$sources
  
  message("📋 Project: ", project_name)
  message("📂 Processing ", length(sources), " data sources...")
  
  # Process each source
  success_count <- 0
  all_temp_files <- character(0)
  
  tryCatch({
    for (source_name in names(sources)) {
      source_config <- sources[[source_name]]
      result <- process_source(source_name, source_config)
      
      if (result$success) {
        success_count <- success_count + 1
      }
      
      all_temp_files <- c(all_temp_files, result$temp_files)
    }
  }, finally = {
    # Cleanup temporary files
    cleanup_temp_files(all_temp_files)
  })
  
  message("\n✨ Pipeline complete!")
  message("⏰ End time: ", Sys.time())
  message("📊 Successfully processed: ", success_count, "/", length(sources), " sources")
  
  if (success_count == length(sources)) {
    message("🎉 All sources processed successfully!")
  } else {
    failed_count <- length(sources) - success_count
    message("⚠️  ", failed_count, " source(s) failed to process")
  }
}

# Test function for individual sources
test_single_source <- function(manifest_path, source_name) {
  message("🧪 Testing single source: ", source_name)
  
  manifest <- load_manifest(manifest_path)
  
  if (!source_name %in% names(manifest$sources)) {
    stop("Source '", source_name, "' not found in manifest")
  }
  
  source_config <- manifest$sources[[source_name]]
  result <- process_source(source_name, source_config)
  
  # Cleanup
  cleanup_temp_files(result$temp_files)
  
  return(result$success)
}

message("✅ Data ingestion pipeline loaded successfully!")
message("📚 Available functions:")
message("   • process_manifest('nyc_pci_sources.yml') - Run full pipeline")
message("   • test_single_source('nyc_pci_sources.yml', 'source_name') - Test one source")