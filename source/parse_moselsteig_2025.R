library(sf)
library(readr)

lapply(list.files(file.path("source", "R"), full.names = TRUE), source)


# Data sources
data_path <- file.path("data", "moselsteig_2025")

# Read track data
track_data_raw <- read_delim(
  file.path(data_path, "track_data.csv"),
  col_types = cols(date = col_date("%d/%m/%Y")),
  show_col_types = FALSE
)

# Calculate ascent and descent
elevation_df <- calculate_elevation(data_path, quiet = TRUE)

# Add elevation to track_data
track_data <- track_data_raw %>%
  left_join(elevation_df, by = join_by(gpx_file))

# Merge gpx files by date
sf_tracks <- merge_gpx_tracks(
  data_path,
  track_data,
  quiet = TRUE
)

# Check
mapview::mapview(sf_tracks, zcol = "label", layer = "Legende")

st_write(sf_tracks, file.path("output", "tracks_moselsteig_2025.gpkg"),
         append = FALSE)
