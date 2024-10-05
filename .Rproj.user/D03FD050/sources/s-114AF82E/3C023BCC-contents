# Top ---------------------------------------------------------------------
# Started Oct 10th 2024
# Terrell Roulston
# Visualization of my bikepack journey across Canada

# My goal is to visualize each segment (day) of riding in a animation
# that can be exported as a video or .gif

# I recorded each ride with my GPS and uploaded the rides to Strava
# I have downloaded all the .GPX files from my activity profile on Strava

# Code adapted from https://martakarass.github.io/post/2022-01-05-gps_strava_read_and_viz/

# Libs --------------------------------------------------------------------
library(tidyverse) # data manipulation and grammar
library(terra) # working w/ spatial data
library(ggplot2) # plotting
library(gganimate) # plot animations
library(XML) # parse XLM/HTML data
library(geosphere) # compute distances and related measures for angular (longitude/latitude) locations.

# Read all GPX files in data folder d
gpx_files <- list.files("C:/Users/terre/Documents/BikeAcrossCanada", pattern = "*.gpx", full.names = TRUE)

# Initialize an empty list to store data frames
data_list <- list()

# Loop through each GPX file
for (file in gpx_files) {
  # Parse the GPX file
  parsed <- htmlTreeParse(file = file, useInternalNodes = TRUE)
  
  # Get values via the respective xpath
  coords <- xpathSApply(parsed, path = "//trkpt", xmlAttrs)
  elev   <- xpathSApply(parsed, path = "//trkpt/ele", xmlValue)
  ts_chr <- xpathSApply(parsed, path = "//trkpt/time", xmlValue)
  
  # Combine into a data frame
  dat_df <- data.frame(
    ts_POSIXct = ymd_hms(ts_chr, tz = "EST"),
    lat = as.numeric(coords["lat",]), 
    lon = as.numeric(coords["lon",]), 
    elev = as.numeric(elev),
    file_name = basename(file)  # Add the file name as a column for reference
  )
  
  # Append the data frame to the list
  data_list[[file]] <- dat_df
}

# Combine all data frames into one
complete_df <- do.call(rbind, data_list)

# View the first few rows of the complete data frame
head(complete_df)

# compute distance (in meters) between subsequent GPS points
complete_df <- 
  complete_df %>%
  mutate(lat_lead = lead(lat)) %>%
  mutate(lon_lead = lead(lon)) %>%
  rowwise() %>%
  mutate(dist_to_lead_m = distm(c(lon, lat), c(lon_lead, lat_lead), fun = distHaversine)[1,1]) %>%
  ungroup()


# plot elevation
plt_elev <- 
  ggplot(complete_df, aes(x = ts_POSIXct, y = elev)) + 
  geom_line() + 
  labs(x = "Time", y = "Elevation [m]") + 
  theme_grey(base_size = 14)
plt_elev

# plot speed
plt_speed_km_per_h <- 
  ggplot(complete_df, aes(x = ts_POSIXct, y = speed_km_per_h)) + 
  geom_line() + 
  labs(x = "Time", y = "Speed [km/h]") + 
  theme_grey(base_size = 14)
plt_speed_km_per_h
