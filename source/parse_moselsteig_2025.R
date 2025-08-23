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
sf_tracks <- process_gpx_activities(
  data_path,
  track_data,
  quiet = TRUE
)

# Check
mapview::mapview(sf_tracks, zcol = "label", layer = "Legende")

# ggplot maps
library(ggmap)

make_bbox_buffer <- function(sf_obj, lon_buffer_m = 1000, lat_buffer_m = 1000) {
  # Ensure WGS84
  sf_obj <- st_transform(sf_obj, 4326)
  
  # Find suitable UTM zone from centroid
  centroid <- st_centroid(st_union(sf_obj))
  lon <- st_coordinates(centroid)[1]
  utm_zone <- floor((lon + 180) / 6) + 1
  crs_utm <- paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m +no_defs")
  
  # Transform to UTM
  sf_utm <- st_transform(sf_obj, crs_utm)
  bbox <- st_bbox(sf_utm)
  
  # Apply buffer in meters
  bbox_exp <- c(
    xmin   = bbox[["xmin"]] - lon_buffer_m,
    ymin = bbox[["ymin"]] - lat_buffer_m,
    xmax  = bbox[["xmax"]] + lon_buffer_m,
    ymax    = bbox[["ymax"]] + lat_buffer_m
  )
  
  # Convert back to WGS84
  bbox_poly <- st_as_sfc(st_bbox(bbox_exp, crs = st_crs(sf_utm)))
  bbox_poly_wgs84 <- st_transform(bbox_poly, 4326)
  out_box <- st_bbox(bbox_poly_wgs84)
  names(out_box) <- c("left", "bottom", "right", "top")
  
  return(out_box)
}

# Download Stamen
basemap <- get_stadiamap(
  bbox = make_bbox_buffer(sf_tracks, 5000, 1000),
  zoom = 12
)

sf_tracks <- sf_tracks %>%
  mutate(geometry = st_cast(geometry, "MULTILINESTRING"))

ggmap(basemap) +
  geom_sf(
    data = sf_tracks,
    aes(color = label),
    linewidth = 1,
    inherit.aes = FALSE
  ) +
  scale_color_viridis_d(name = "Day") +
  guides(color = guide_legend(override.aes = list(linewidth = 2))) +
  ggspatial::annotation_scale(location = "br", style = "ticks",
                              width_hint = 0.4, line_width = 3, text_cex = 1) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    style = north_arrow_fancy_orienteering
  ) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.98, 0.1),
    legend.justification = c(1, 0),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.background = element_rect(fill = alpha("white", 0.75)))

ggsave(file.path("output", "moselsteig_2025_plot1.png"), dpi = 300)

## osm
bbox <- make_bbox_buffer(sf_tracks, 5000, 1000)
names(bbox) <- c("xmin", "ymin", "xmax", "ymax")
tls <- maptiles::get_tiles(
  x = bbox,
  provider = "OpenStreetMap",
  zoom = 12)

ggplot() +
  tidyterra::geom_spatraster_rgb(data = tls, maxcell = 1e10) +
  geom_sf(
    data = sf_tracks,
    aes(color = label),
    linewidth = 1,
    inherit.aes = FALSE
  ) +
  scale_color_viridis_d(name = "Day") +
  guides(color = guide_legend(override.aes = list(linewidth = 2))) +
  coord_sf(xlim = c(bbox[["xmin"]], bbox[["xmax"]]),
           ylim = c(bbox[["ymin"]], bbox[["ymax"]])) +
  ggspatial::annotation_scale(location = "br", style = "ticks",
                              width_hint = 0.4, line_width = 3, text_cex = 1) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    style = north_arrow_fancy_orienteering
  ) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.98, 0.1),
    legend.justification = c(1, 0),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.background = element_rect(fill = alpha("white", 0.75)))

ggsave(file.path("output", "moselsteig_2025_plot2.png"), dpi = 300)

st_write(sf_tracks, file.path("output", "tracks_moselsteig_2025.gpkg"),
         append = FALSE)
