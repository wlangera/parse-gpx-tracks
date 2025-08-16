library(sf)
library(readr)

lapply(list.files(file.path("source", "R"), full.names = TRUE), source)


# Data sources
data_path <- file.path("data", "czech_republic_2025")

# Read track data
track_data <- read_delim(
  file.path(data_path, "track_data.csv"),
  col_types = cols(date = col_date("%d/%m/%Y")),
  show_col_types = FALSE
)

# Merge gpx files by date
sf_tracks <- process_gpx_activities(
  data_path,
  track_data,
  quiet = TRUE
)

# Check
mapview::mapview(sf_tracks, zcol = "label", layer = "Legende")

st_write(sf_tracks, file.path("output", "tracks_czech_republic_2025.gpkg"),
         append = FALSE)
