# *** Make sure your environment is cleared *** #

library(dplyr)
library(stringr)
library(sf)
 
# Setting up the environment and calling files
wf <- function(x) paste('https://raw.githubusercontent.com/kbratley/saguaro/main/', x, sep='')

# List of file names
files <- c("Data/2016_variables.csv", "Data/2017_variables.csv", "Data/2018_variables.csv", 
           "Data/2019_variables.csv", "Data/2021_variables.csv", "Data/2022_variables.csv")

# Load the final matching shapefile for clipping
clip_shapefile <- st_read("Results/final_combined_dataset.shp")

# Initialize an empty list to store clipped datasets
clipped_datasets <- list()

# Combine all files and clip each extent to aggregated matched results
for (i in 1:length(files)) {
  # Read the file
  print(paste("Reading file:", files[i]))
  combined_data <- read.csv(files[i])
  
  # Add a column for the year, extracted from the file name
  combined_data$Year <- as.numeric(str_extract(files[i], "\\d+"))
  
  # Convert data to an sf object
  combined_data <- st_as_sf(combined_data, coords = c("longitude", "latitude"))
  
  # Ensure both datasets have the same CRS
  st_crs(combined_data) <- st_crs(clip_shapefile)
  
  # Clip the data using the shapefile
  combined_data <- st_intersection(combined_data, clip_shapefile)
  
  # Extract 'latitude' and 'longitude' columns
  coordinates <- st_coordinates(combined_data)
  combined_data <- cbind(combined_data, longitude = coordinates[, "X"], latitude = coordinates[, "Y"])
  
  # Store clipped data in the list
  clipped_datasets[[i]] <- as.data.frame(combined_data)
  cat("Processed file", i, "of", length(files), "\n")
}

# Combine all clipped datasets into a single data frame
combined_data <- do.call(rbind, clipped_datasets)

# Create a unique pixel ID based on lat and lon
combined_data <- combined_data %>%
  group_by(latitude, longitude) %>%
  mutate(Pixel_ID = group_indices())

# Integrate the provided code to filter out rows with less than 6 corresponding rows for each 'Pixel_ID'
pixel_counts <- combined_data %>%
  group_by(Pixel_ID) %>%
  summarise(row_count = n())

pixels_to_remove <- pixel_counts %>%
  filter(row_count < 6) %>%
  pull(Pixel_ID)

combined_data <- combined_data %>%
  filter(!(Pixel_ID %in% pixels_to_remove))

# Function to create new columns for sine and cosine of aspect with a 45-degree shift
transform_aspect <- function(data, aspect_col_name) {
  data %>%
    mutate(eastness = sin((!!sym(aspect_col_name)-45)),
           aspect_transform = cos((!!sym(aspect_col_name)-45)))
}
combined_data <- transform_aspect(combined_data, "aspect")

# Add a new column that indicates whether each data point was sprayed in the previous year
combined_data <- combined_data %>%
  group_by(Pixel_ID) %>%
  mutate(treated_Prev1Y = ifelse(
    Year %in% c(2022, 2019, 2018, 2017), lag(treated),
    ifelse(Year %in% c(2021, 2016), 0, treated)
  ))

# Add a new column that indicates whether each data point was sprayed two years prior
combined_data <- combined_data %>%
  group_by(Pixel_ID) %>%
  mutate(treated_Prev2Y = ifelse(
    Year %in% c(2021, 2019, 2018), lag(treated, n=2),
    ifelse(Year %in% c(2022, 2017, 2016), 0, treated)
  ))

# # Organize data match question formats:
# # Treated in the current year and previous year 
# combined_data$treated_2yrs_consecutively <- ifelse(combined_data$treated == 1 & combined_data$treated_Prev1Y == 1, 1, 0)
# 
# # Treated in previous year but not current
# combined_data$treated_1yrPrior_notFollowingYr <- ifelse(combined_data$treated == 0 & combined_data$treated_Prev1Y == 1, 1, 0)

# Organize the column order
panel_data <- combined_data[, c("Pixel_ID", "Year", "greennessChange",
                           "treated", #"treated_1yrPrior_notFollowingYr", "treated_2yrs_consecutively",
                           "aspect_transform", "elevation", "slope", 
                           "preSprayGreenness", "postSprayGreenness", 
                           "latitude", "longitude")]

write.csv(panel_data, "Results/panel_data.csv", row.names = FALSE)
