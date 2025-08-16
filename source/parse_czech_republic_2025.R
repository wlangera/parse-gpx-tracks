library(sf)
source(file.path("source", "merge_gpx_tracks.R"))

# Data sources
data_path <- file.path("data", "czech_republic_2025")
track_file <- "track_data.csv"

# Merge gpx files by date
sf_tracks <- merge_gpx_tracks(
  data_path,
  track_file,
  col_types = cols(date = col_date("%d/%m/%Y"))
)

# Check
mapview::mapview(sf_tracks, zcol = "label", layer = "Legende")

st_write(sf_tracks, file.path("output", "tracks_czech_republic_2025.gpkg"),
         append = FALSE)
