library(gpx)
library(terra)
library(tidyverse)
library(tidyterra)
library(geosphere)
library(lubridate)
library(geodata)
library(RColorBrewer)

# Create a list of .gpx file path names
gpx_files <- list.files("C:/Users/terre/Documents/BikeAcrossCanada", pattern = "*.gpx", full.names = TRUE)
# Order the files names in order of the day of the trip
gpx_files <- gpx_files[order(as.numeric(sub(".*Day_(\\d+).*", "\\1", gpx_files)))]

# Read .gpx files and create a list of lists with track, waypoint and route data
# We are only going to work with the track 
gpx_data <- lapply(gpx_files, read_gpx)

# Assuming gpx_data is a list of GPX files
summarized_tracks <- map_dfr(1:length(gpx_data), function(i) {
  track_data_list <- gpx_data[[i]]$tracks
  track_data_df <- bind_rows(track_data_list)
  
  # Ensure Time is in the correct format
  track_data_df <- track_data_df %>%
    mutate(Time = parse_date_time(Time, orders = "ymd HMS", tz = "UTC")) %>%
    mutate(
      Distance = c(0, distHaversine(
        cbind(Longitude[-n()], Latitude[-n()]), 
        cbind(Longitude[-1], Latitude[-1])
      ))
    )
  
  # Filter out rows with NA in Time
  summary_stats <- track_data_df %>%
    filter(!is.na(Time)) %>%
    summarize(
      total_distance = sum(Distance, na.rm = TRUE),
      total_time = as.numeric(difftime(max(Time), min(Time), units = "mins"))
    )
  
  # Calculate total elevation gain
  elevation_gain <- track_data_df %>%
    arrange(Time) %>%
    mutate(elevation_change = Elevation - lag(Elevation, default = first(Elevation))) %>%
    filter(elevation_change > 0) %>%
    summarize(total_elevation_gain = sum(elevation_change, na.rm = TRUE))
  
  # Combine results into one data frame
  summary_stats <- cbind(summary_stats, elevation_gain) %>%
    mutate(total_distance_km = total_distance / 1000,
           total_time_hr = total_time / 60) %>%
    select(total_distance_km, total_elevation_gain, total_time_hr)
  
  # Add track identifier based on index
  summary_stats$track_id <- i
  
  return(summary_stats)
})

# View the summarized results
print(summarized_tracks)

sum(summarized_tracks$total_distance_km) # 7846.29 total distance
sum(summarized_tracks$total_elevation_gain) # 58025.4 elevation gain
sum(summarized_tracks$total_time_hr)/24 #32.16 days biking


# Initialize an empty list to store the layers
track_layers <- list()

# Loop through each GPX file and read only the 'tracks' layer
for (file in gpx_files) {
  gpx_layer <- vect(file, layer = "tracks")  # Read only the 'tracks' layer
  track_layers[[file]] <- gpx_layer
}

# Combine all track layers into a single vector
combined_tracks <- do.call(c, track_layers)

plot(combined_tracks$`C:/Users/terre/Documents/BikeAcrossCanada/Day_1_Vancouver_to_Chilliwack_BC_Morning_ride_.gpx`)



setwd('./basemap')

ca_bound <- gadm(country = 'CA', level = 1, resolution = 1,
                 path = '../base_maps')
us_bound <- gadm(country = 'US', level = 0, resolution = 1,
                 path = '../base_maps')

ca_us_bound <- rbind(ca_bound, us_bound)

great_lakes <- vect('C:/Users/terre/Documents/Acadia/Malus Project/maps/great lakes/combined great lakes/')



# Plot Canada and USD
plot(ca_bound, xlim = c(-150, -48), ylim = c(40, 60), col = 'snow2', background = 'lightblue2')
plot(us_bound, xlim = c(-150, -48), ylim = c(40, 60), col = 'snow3', background = 'lightblue2', add = T)
# Plot great lakes
plot(great_lakes, add = T, col = 'lightblue2')

# Plot the combined track layers
# Loop through the rest of the layers and add them to the plot

for (i in 1:length(track_layers)) {
  plot(track_layers[[i]], col = 'red', add = TRUE, lwd = 3)
}





