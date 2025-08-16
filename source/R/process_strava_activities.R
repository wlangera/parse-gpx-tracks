#' Process Strava activity list into an `sf` object with ascent and descent
#'
#' This function takes a list of Strava activities (as returned by
#' [rStrava::get_activity_list()]) and a Strava token, then:
#' \itemize{
#'   \item decodes the `summary_polyline` for each activity into an `sf` LINESTRING,
#'   \item extracts metadata (date, distance, ascent),
#'   \item downloads altitude streams for each activity,
#'   \item calculates cumulative ascent and descent from the altitude stream,
#'   \item rescales descent to match Strava's reported ascent,
#'   \item returns a tidy `sf` object with one row per activity.
#' }
#'
#' @param act_data A list of activities returned by
#'   [rStrava::get_activity_list()]. Each element must include a `map$summary_polyline`.
#' @param stoken A Strava authentication token as returned by
#'   [rStrava::strava_oauth()] or created with [httr::config()].
#'
#' @return An `sf` object with one row per activity, containing:
#' \describe{
#'   \item{nr}{Sequential activity number (ordered by date).}
#'   \item{date}{Activity start date (UTC).}
#'   \item{dist}{Distance in kilometers.}
#'   \item{ascent}{Total ascent reported by Strava (meters).}
#'   \item{descent}{Estimated total descent (meters), scaled to match Strava's ascent.}
#'   \item{geometry}{LINESTRING geometry of the activity route in EPSG:4326.}
#' }
#'
#' @details
#' Strava’s API does not provide descent in the activity summary.
#' This function calculates ascent and descent directly from the altitude stream.
#' Descent is then rescaled so that calculated ascent matches Strava’s
#' reported `total_elevation_gain`, making ascent/descent consistent.
#'
#' @examples
#' \dontrun{
#' stoken <- httr::config(token = readRDS('.httr-oauth')[[1]])
#' act_list <- get_activity_list(
#'   stoken,
#'   after  = as.Date("2025-07-17"),
#'   before = as.Date("2025-07-21")
#' )
#' res <- process_strava_activities(act_list, stoken)
#' plot(res["ascent"])
#' }

process_strava_activities <- function(act_data, stoken) {
  require("rStrava")
  require("googlePolylines")
  require("sf")
  require("dplyr")
  
  # Get summary info with polylines
  activity_summary <- lapply(act_data, function(activity) {
    poly <- activity$map$summary_polyline
    
    # decode to lat/lon
    coords <- googlePolylines::decode(poly)[[1]]
    
    st_sf(
      id = as.character(activity$id),
      date = as.Date(activity$start_date),
      dist = activity$distance / 1000,
      ascent = activity$total_elevation_gain,
      geometry = st_sfc(st_linestring(cbind(coords$lon, coords$lat)),
                        crs = 4326)
    )
  }) %>%
    bind_rows()
  
  # Get streams for elevation calculation
  streams <- get_activity_streams(act_data, stoken)
  
  elevation_df <- streams %>%
    group_by(id) %>%
    summarise(
      ascent2 = calculate_elevation_gain(data.frame(ele = altitude)),
      descent2 = calculate_elevation_gain(
        data.frame(ele = altitude),
        ascent = FALSE
      ),
      .groups = "drop"
    )
  
  # Join and adjust
  out_df <- activity_summary %>%
    full_join(elevation_df, by = join_by(id)) %>%
    arrange(date) %>%
    mutate(
      p = ascent / ascent2,
      descent = descent2 * p,
      nr = row_number(),
      dist_label =
        paste0(
          round(dist, 1), " km (",
          round(ascent, 1), " m ↑, ",
          round(descent, 1), " m ↓)"
        ),
      label =
        paste0(
          nr, ": ", dist_label
        )
    ) %>%
    select("nr", "date", "dist", "ascent", "descent", "dist_label", "label",
           "geometry")
  
  return(out_df)
}
