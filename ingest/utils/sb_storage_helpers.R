'
Kalli A. Hale

sb_storage_helpers.R

This file contains helper functions for
  navigating Supabase storage.
'

pacman::p_load(httr2, yaml)

# in sb_storage_helpers.R

supa_upload <- function(local_file_path, object_path) {
  #' @param local_file_path The path to the file you want to upload.
  #' @param object_path The destination path and file name in the bucket.
  
  # load project API securely
  secrets <- yaml::read_yaml("config/ingredients/secrets.yml")
  sb_url <- secrets$supabase$url
  sb_key <- secrets$supabase$service_role_key
  sb_bucket <- secrets$supabase$bucket_name
  
  # construct the API endpoint URL
  api_url <- paste0(sb_url, "/storage/v1/object/", sb_bucket, "/", object_path)
  
  # create request
  req <- request(api_url) |>
    req_auth_bearer_token(token = sb_key) |>
    req_headers(`x-upsert` = "true") |>
    req_method("POST") |> 
    req_body_file(path = local_file_path)
  
  # perform the request
  resp <- req_perform(req)
  
  # check for success
  if (resp_status(resp) == 200) {
    message("Successfully uploaded ", local_file_path, " to ", object_path)
  } else {
    # --- IMPROVEMENT: Capture and print the detailed error message ---
    error_status <- resp_status(resp)
    error_message <- tryCatch({
      resp_body_json(resp)
    }, error = function(e) {
      list(message = "Could not parse error response body.")
    })
    
    # Construct a more informative error
    stop(
      "Upload failed with status: ", error_status, "\n",
      "Supabase error: ", error_message$message
    )
  }
  
  return(resp)
}
supa_download <- function(object_path, local_destination) {
  #' @param object_path The path of the object in the bucket.
  #' @param local_destination The local file path to save the object to.
  
  secrets <- yaml::read_yaml("config/secrets.yml")
  sb_url <- secrets$supabase$url
  sb_key <- secrets$supabase$service_role_key
  sb_bucket <- secrets$supabase$bucket_name
  
  api_url <- paste0(sb_url, "/storage/v1/object/", sb_bucket, "/", object_path)
  
  req <- request(api_url) |>
    req_auth_bearer_token(token = sb_key)
  
  # perform request, writing the response body directly to a file
  resp <- req_perform(req, path = local_destination)
  
  if (resp_status(resp) == 200) {
    message("Successfully downloaded ", object_path, " to ", local_destination)
  } else {
    stop("Download failed with status: ", resp_status(resp))
  }
  
  return(local_destination)
}