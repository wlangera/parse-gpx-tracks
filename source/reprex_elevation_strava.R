library(rStrava)

# Get Strava activity
stoken <- httr::config(token = readRDS('.httr-oauth')[[1]])
activity <- get_activity_list(
  stoken,
  after = as.Date("2025-07-18"),
  before = as.Date("2025-07-20")
)


## Calculation like in issue post
(ascent <- activity[[1]]$total_elevation_gain)
(descent <- ascent - activity[[1]]$elev_high + activity[[1]]$elev_low)


## Manual calculation from stream
# Get stream data
stream <- get_activity_streams(activity, stoken)

# Calculate the difference between consecutive elevation points
elevation_diff <- diff(stream$altitude)

# Total ascent
sum(elevation_diff[elevation_diff > 0], na.rm = TRUE)

# Total descent
sum(abs(elevation_diff[elevation_diff < 0]), na.rm = TRUE)
