#' Calculate Ascent and Descent from GPX Track Points
#'
#' Reads GPX track point data from a folder and calculates total ascent and descent 
#' for each track file.
#'
#' @param data_path Character string. Path to the folder containing GPX files.
#' @param layer Character string. GPX layer to read for points (default = `"track_points"`).
#' @param elevation_col Character string. Name of the column containing elevation values (default = `"ele"`).
#' @param ... Additional arguments passed to [sf::st_read()].
#'
#' @details 
#' For each GPX file in `data_path`, the function reads the specified layer and 
#' computes the total ascent and descent using [calculate_elevation_gain()]. The 
#' output is a data frame with one row per track file.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{name}{File name of the GPX track (without extension).}
#'   \item{ascent}{Total ascent (positive elevation gain) in the track.}
#'   \item{descent}{Total descent (negative elevation change) in the track.}
#' }
#'
#' @examples
#' \dontrun{
#' # Calculate ascent/descent for all tracks in folder
#' elevation_summary <- calculate_elevation("data/moselsteig_2025")
#' }

calculate_elevation <- function(
    data_path,
    layer = "track_points",
    elevation_col = "ele",
    ...) {
  require("sf")
  require("dplyr")

  # Get GPX files
  gpx_files <- list.files(
    data_path,
    pattern = "\\.gpx$",
    full.names = TRUE
  )

  # Read all GPX tracks
  gpx_list <- lapply(gpx_files, read_gpx_file, layer = layer, ...)
  
  # Calculate ascent, descent
  elevation_list <- lapply(gpx_list, function(gpx) {
    data.frame(
      ascent = calculate_elevation_gain(
        gpx,
        ascent = TRUE,
        elevation_col = elevation_col
      ),
      descent = calculate_elevation_gain(
        gpx,
        ascent = FALSE,
        elevation_col = elevation_col
      )
    )
  })
  
  # Combine gpx geometries in single sf object
  bind_rows(elevation_list) %>%
    mutate(
      gpx_file = gsub(paste0(data_path, "/"), "", gpx_files),
      gpx_file = gsub("\\.gpx", "", gpx_file)
    ) 
}