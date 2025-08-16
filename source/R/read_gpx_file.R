#' Helper function to read a GPX file safely
#'
#' Reads a GPX file and automatically selects a layer if available.
#' Defaults to `"tracks"` layer if it exists, otherwise reads the first layer.
#'
#' @param file Character. Path to the GPX file.
#' @param layer Character. Layer name in case layer is missing, read_gpx_file
#' will read the first layer.
#' @param ... Additional arguments passed to [sf::st_read()].
#'
#' @return An `sf` object containing the GPX layer.
#' @keywords internal
read_gpx_file <- function(file, layer, ...) {
  require(sf)
  
  layers <- st_layers(file)$name
  
  if (layer %in% layers) {
    st_read(file, layer = layer, ...)
  } else {
    st_read(file, ...)
  }
}
