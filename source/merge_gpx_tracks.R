#' Merge GPX Tracks and Metadata
#'
#' Reads multiple GPX track files and a corresponding metadata file (CSV or other 
#' delimiter-separated format), joins them, and merges tracks by date. For each date, 
#' distances, ascents, and descents are summed, start and end points are determined 
#' from the first and last stages, geometries are unioned, and a formatted label is 
#' created.
#'
#' @param data_path Character string. Path to the folder containing GPX files and the metadata file.
#' @param track_file Character string. Name of the metadata file (e.g., `"track_data.csv"`).
#' @param gpx_col Character string. Name of the column in the metadata that contains 
#'   the GPX file identifiers (default = `"gpx_file"`).
#' @param date_col Character string. Name of the column with track dates (default = `"date"`).
#' @param start_col Character string. Name of the column with track start locations (default = `"start"`).
#' @param end_col Character string. Name of the column with track end locations (default = `"end"`).
#' @param ascent_col Character string. Name of the column with ascent values (default = `"ascent"`).
#' @param descent_col Character string. Name of the column with descent values (default = `"descent"`).
#' @param nr_col Character string or `NULL`. Name of the column with stage order. 
#'   If `NULL` or the column does not exist, a new order column is created by sorting 
#'   rows by date and assigning sequential numbers (default = `NULL`).
#' @param ... Additional arguments passed to [readr::read_delim()] when reading the metadata file.
#'
#' @details 
#' Within each date group, the function:
#' \itemize{
#'   \item takes the start from the lowest stage number (`nr_col`),
#'   \item takes the end from the highest stage number,
#'   \item sums distance, ascent, and descent,
#'   \item unions geometries with [sf::st_union()],
#'   \item generates labels of the form `"1: start - end\nXX km (YY m ↑, ZZ m ↓)"`.
#' }
#'
#' @return An `sf` object with merged tracks, including the following columns:
#'   \item{nr}{Sequential stage number per merged track (ordered by date).}
#'   \item{start}{Start location.}
#'   \item{end}{End location.}
#'   \item{date}{Date of the track.}
#'   \item{dist}{Total distance (numeric).}
#'   \item{ascent}{Total ascent (numeric).}
#'   \item{descent}{Total descent (numeric).}
#'   \item{dist_label}{Formatted label for distance and elevation.}
#'   \item{label}{Combined label for mapping/plotting.}
#'   \item{geometry}{Unioned track geometry.}
#'
#' @examples
#' \dontrun{
#' sf_merged <- merge_gpx_tracks(
#'   data_path = "data/czech_republic_2025",
#'   track_file = "track_data.csv",
#'   gpx_col = "gpx_file",
#'   date_col = "date",
#'   start_col = "start",
#'   end_col = "end",
#'   ascent_col = "ascent",
#'   descent_col = "descent",
#'   nr_col = "nr"
#' )
#' }
merge_gpx_tracks <- function(
    data_path,
    track_file,
    gpx_col = "gpx_file",
    date_col = "date",
    start_col = "start",
    end_col = "end",
    ascent_col = "ascent",
    descent_col = "descent",
    nr_col = NULL,
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
  
  # Read metadata
  track_data <- read_delim(file.path(data_path, "track_data.csv"),
                           show_col_types = FALSE, ...)
  
  # Read all GPX tracks
  gpx_list <- lapply(gpx_files, st_read, layer = "tracks", quiet = TRUE)
  
  sf_table <- bind_rows(gpx_list) %>%
    mutate(
      name = gsub(paste0(data_path, "/"), "", gpx_files),
      name = gsub("\\.gpx", "", name)
    ) %>%
    select(where(~ !any(is.na(.x)))) %>%
    left_join(track_data, by = c("name" = gpx_col))
  
  # Check if nr_col exists, otherwise create one
  if (is.null(nr_col) || !(nr_col %in% names(sf_table))) {
    nr_col <- "nr"
    sf_table <- sf_table %>%
      arrange(.data[[date_col]]) %>%
      mutate("{nr_col}" := row_number())
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
    arrange(.data[[date_col]]) %>%
    mutate(
      nr = row_number(),
      dist_label = paste0(
        round(dist, 1), " km (",
        round(ascent, 1), " m ↑, ",
        round(descent, 1), " m ↓)"
      ),
      label = paste0(
        nr, ": ", start, " - ", end, "\n", dist_label
      )
    ) %>%
    select("nr", everything()) %>%
    relocate("geometry", .after = last_col())
  
  return(sf_merged)
}
