library(rStrava)
library(dplyr)
library(sf)

lapply(list.files(file.path("source", "R"), full.names = TRUE), source)


# Get Strava activities
stoken <- httr::config(token = readRDS('.httr-oauth')[[1]])
activities <- get_activity_list(
  stoken,
  after = as.Date("2025-04-28"),
  before = as.Date("2025-05-21")
)

# Get summary info with polylines
activity_summary <- lapply(activities, function(activity) {
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

act_df <- compile_activities(activities)

act_df %>%
  mutate(googlePolylines::decode(act_df[1,]$map.summary_polyline)[[1]][, 1])
