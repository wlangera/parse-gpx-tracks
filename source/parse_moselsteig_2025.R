library(sf)
source(file.path("source", "R", "merge_gpx_tracks.R"))

# Data sources
data_path <- file.path("data", "moselsteig_2025")

# Read track data
track_data <- read_delim(
  file.path(data_path, "track_data.csv"),
  col_types = cols(date = col_date("%d/%m/%Y")),
  show_col_types = FALSE
)

# Calculate ascent and descent

# Merge gpx files by date
sf_tracks <- merge_gpx_tracks(
  data_path,
  track_data,
  quiet = TRUE
)

# Check
mapview::mapview(sf_tracks, zcol = "label", layer = "Legende")

st_write(sf_tracks, file.path("output", "tracks_czech_republic_2025.gpkg"),
         append = FALSE)
