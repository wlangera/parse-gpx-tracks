#' Merge GPX Tracks with Metadata
#'
#' Reads GPX track files from a folder, joins them with a user-provided metadata 
#' data frame, and merges tracks by date. For each date, distances, ascents, and 
#' descents are summed, start and end points are determined from the first and last 
#' stages, geometries are unioned, and formatted labels are created.
#'
#' @param data_path Character string. Path to the folder containing GPX files.
#' @param track_df A `data.frame` (or tibble) containing metadata for the GPX files. 
#'   Must contain at least a column matching `gpx_col` and the specified `date_col`, 
#'   `start_col`, `end_col`, and optionally `nr_col`.
#' @param layer Character string. GPX layer to read for points (default = `"tracks"`).
#' @param gpx_col Character string. Column name in `track_df` that contains GPX file 
#'   identifiers (default = `"gpx_file"`). These should match the GPX file names 
#'   (without extension) in `data_path`.
#' @param date_col Character string. Column name with track dates (default = `"date"`).
#' @param start_col Character string. Column name with start locations (default = `"start"`).
#' @param end_col Character string. Column name with end locations (default = `"end"`).
#' @param ascent_col Character string. Column name with ascent values (default = `"ascent"`).
#' @param descent_col Character string. Column name with descent values (default = `"descent"`).
#' @param nr_col Character string or `NULL`. Column name with stage order. If `NULL` 
#'   or not found in `track_df`, a new sequential column is created by sorting by 
#'   `date_col` (default = `NULL`).
#' @param ... Additional arguments passed to [sf::st_read()] when reading GPX files.
#'
#' @details 
#' For each date, the function:
#' \itemize{
#'   \item selects the start from the lowest stage number (`nr_col`),
#'   \item selects the end from the highest stage number,
#'   \item sums distance, ascent, and descent,
#'   \item unions geometries with [sf::st_union()],
#'   \item generates labels of the form 
#'   \code{"1: start - end\nXX km (YY m ↑, ZZ m ↓)"} if ascent/descent are available, 
#'   otherwise \code{"1: start - end (XX km)"}.
#' }
#'
#' @return An `sf` object with merged tracks, including:
#' \describe{
#'   \item{nr}{Sequential stage number across all merged dates.}
#'   \item{start}{Start location.}
#'   \item{end}{End location.}
#'   \item{date}{Date of the merged track.}
#'   \item{dist}{Total distance.}
#'   \item{ascent}{Total ascent (if provided).}
#'   \item{descent}{Total descent (if provided).}
#'   \item{dist_label}{Formatted distance/elevation summary.}
#'   \item{label}{Label suitable for plotting or mapping.}
#'   \item{geometry}{Unioned track geometry.}
#' }
#'
#' @examples
#' \dontrun{
#' library(sf)
#' 
#' # Data sources
#' data_path <- file.path("data", "moselsteig_2025")
#' 
#' # Read track metadata
#' track_data <- readr::read_delim(
#'   file.path(data_path, "track_data.csv"),
#'   col_types = readr::cols(date = readr::col_date("%d/%m/%Y")),
#'   show_col_types = FALSE
#' )
#'
#' # Merge GPX files by date
#' sf_tracks <- merge_gpx_tracks(
#'   data_path = data_path,
#'   track_df = track_data
#' )
#' }

merge_gpx_tracks <- function(
    data_path,
    track_df,
    layer = "tracks",
    gpx_col = "gpx_file",
    date_col = "date",
    start_col = "start",
    end_col = "end",
    nr_col = "nr",
    ascent_col = "ascent",
    descent_col = "descent",
    ...
) {
  require("sf")
  require("dplyr")
  require("rlang")
  require("readr")

  # Get GPX files
  gpx_files <- list.files(
    data_path,
    pattern = "\\.gpx$",
    full.names = TRUE
  )
  
  # Read all GPX tracks
  gpx_list <- lapply(gpx_files, read_gpx_file, layer = layer, ...)
  
  # Combine gpx geometries in single sf object
  sf_table <- bind_rows(gpx_list) %>%
    mutate(
      name = gsub(paste0(data_path, "/"), "", gpx_files),
      name = gsub("\\.gpx", "", name)
    ) %>%
    select(where(~ !any(is.na(.x)))) %>%
    left_join(track_df, by = c("name" = gpx_col))
  
  # Check if nr_col exists, otherwise create one
  if (!(nr_col %in% names(sf_table))) {
    sf_table <- sf_table %>%
      arrange(.data[[date_col]]) %>%
      mutate("{nr_col}" := row_number())
  }
  # Check if ascent, descent columns exists, otherwise create them
  if (!all(c(ascent_col, descent_col) %in% names(sf_table))) {
    sf_table <- sf_table %>%
      mutate("{ascent_col}" := 0,
             "{descent_col}" := 0)
  }
  
  # Summarise per date
  sf_merged <- sf_table %>%
    group_by(.data[[date_col]]) %>%
    summarise(
      start   = .data[[start_col]][which.min(.data[[nr_col]])],
      end     = .data[[end_col]][which.max(.data[[nr_col]])],
      dist    = sum(.data[["dist"]], na.rm = TRUE),
      ascent  = sum(.data[[ascent_col]], na.rm = TRUE),
      descent = sum(.data[[descent_col]], na.rm = TRUE),
      geometry = st_union(geometry),
      .groups = "drop"
    ) %>%
    arrange(.data[[date_col]])
  
  # Create labels and make nice output
  out_sf <- sf_merged %>%
    mutate(
      nr = row_number(),
      dist_label = if (sum(sf_merged[[ascent_col]]) == 0 &
                       sum(sf_merged[[descent_col]]) == 0) {
          paste(round(dist, 1), "km")
        } else {
          paste0(
            round(dist, 1), " km (",
            round(ascent, 1), " m ↑, ",
            round(descent, 1), " m ↓)"
          )
        },
      label = if (sum(sf_merged[[ascent_col]]) == 0 &
                  sum(sf_merged[[descent_col]]) == 0) {
        paste0(
          nr, ": ", start, " - ", end, " (", dist_label, ")"
        )
      } else {
        paste0(
          nr, ": ", start, " - ", end, "\n", dist_label
        )
      }
    ) %>%
    select("nr", everything()) %>%
    relocate("geometry", .after = last_col())
  
  return(out_sf)
}
