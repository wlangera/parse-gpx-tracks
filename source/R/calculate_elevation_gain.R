#' Calculate Total Ascent or Descent from Elevation Vector
#'
#' Computes total elevation gain (ascent) or loss (descent) from a vector of 
#' consecutive elevation measurements.
#'
#' @param df A data frame or tibble containing an elevation column.
#' @param ascent Logical. If TRUE, calculate total ascent (positive gains). If FALSE, calculate total descent (negative losses) (default = TRUE).
#' @param elevation_col Character string. Name of the column containing elevation values (default = `"ele"`).
#'
#' @details
#' The function calculates the difference between consecutive elevation points. 
#' Depending on `ascent`, it sums either all positive differences (ascent) or the 
#' absolute value of all negative differences (descent).
#'
#' @return Numeric. Total ascent or descent in the same units as `elevation_col`.
#'
#' @examples
#' \dontrun{
#' # Example elevation vector
#' track_data <- data.frame(ele = c(100, 120, 110, 130))
#' calculate_elevation_gain(track_data, ascent = TRUE)  # returns 40
#' calculate_elevation_gain(track_data, ascent = FALSE) # returns 20
#' }


calculate_elevation_gain <- function(df, ascent = TRUE, elevation_col = "ele") {
  # Calculate the difference between consecutive elevation points
  elevation_diff <- diff(df[[elevation_col]])
  
  if (ascent) {
    # Total ascent: sum of all positive elevation differences
    return(sum(elevation_diff[elevation_diff > 0], na.rm = TRUE))
  }
  return(sum(abs(elevation_diff[elevation_diff < 0]), na.rm = TRUE))
}
