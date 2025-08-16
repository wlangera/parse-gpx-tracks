library(rStrava)

lapply(list.files(file.path("source", "R"), full.names = TRUE), source)


# Get Strava activities
stoken <- httr::config(token = readRDS('.httr-oauth')[[1]])
activities <- get_activity_list(
  stoken,
  after = as.Date("2025-07-17"),
  before = as.Date("2025-07-21")
)

# Get summary stats from activities
sf_tracks <- process_strava_activities(activities, stoken)

# Check
mapview::mapview(sf_tracks, zcol = "label", layer = "Legende")

st_write(sf_tracks, file.path("output", "tracks_norway_2025_strava.gpkg"),
         append = FALSE)
