library(dplyr)
library(stringr)
library(sf)
library(tidyverse)
library(modelsummary)
library(fixest)

# Setting up the environment and calling files
wf <- function(x) paste('https://raw.githubusercontent.com/kbratley/saguaro/main/', x, sep='')

# List of file names
files <- c("Data/2016_variables.csv", "Data/2017_variables.csv", "Data/2018_variables.csv", 
           "Data/2019_variables.csv", "Data/2021_variables.csv", "Data/2022_variables.csv")

# Load the final matching shapefile for clipping
clip_shapefile <- st_read("Results/matching_results.shp")

##################
# Organize Data
##################

# Initialize an empty list for clipped datasets
clipped_datasets <- list()

# Combine and clip each file to match specified shape
for (i in 1:length(files)) {
  # Read file and add year column
  combined_data <- read.csv(files[i])
  combined_data$Year <- as.numeric(str_extract(files[i], "\\d+"))
  
  # Convert to sf object and clip
  combined_data <- st_as_sf(combined_data, coords = c("longitude", "latitude"))
  combined_data <- st_intersection(combined_data, clip_shapefile)
  
  # Extract latitude and longitude columns
  coordinates <- st_coordinates(combined_data)
  combined_data <- cbind(combined_data, longitude = coordinates[, "X"], latitude = coordinates[, "Y"])
  
  # Store clipped data
  clipped_datasets[[i]] <- as.data.frame(combined_data)
  cat("Processed file", i, "of", length(files), "\n")
}

# Combine all clipped datasets
combined_data <- do.call(rbind, clipped_datasets)

# Create a unique pixel ID based on lat and lon
combined_data <- combined_data %>%
  group_by(latitude, longitude) %>%
  mutate(Pixel_ID = cur_group_id())

# Filter out rows with less than 6 corresponding rows for each 'Pixel_ID'
pixel_counts <- combined_data %>%
  group_by(Pixel_ID) %>%
  summarise(row_count = n())

pixels_to_remove <- pixel_counts %>%
  filter(row_count < 6) %>%
  pull(Pixel_ID)

combined_data <- combined_data %>%
  arrange(Pixel_ID)

# Ensure that Pixel_ID is sequential and has no missing #'s
# (Kind of a backwards way to do this- but computationally easy)
num_rows <- nrow(combined_data)
num_groups <- ceiling(num_rows / 6) #calculate # of groups
new_id_sequence <- rep(1:num_groups, each = 6) #create sequence of #'s repeating every 6 rows
new_id_sequence <- rep(new_id_sequence, length.out = num_rows)  # Repeat the sequence to cover all rows

# Remove the old Pixel_ID column and update it to reflect the new_id_sequence
combined_data <- combined_data[, !names(combined_data) %in% "Pixel_ID"]
combined_data$Pixel_ID <- new_id_sequence

# New dataframe to prevent errors
panel_data <- combined_data

panel_data <- panel_data %>%
  pivot_longer(cols = starts_with("preSprayGreenness") | starts_with("postSprayGreenness"),
               names_to = "post_treatment", values_to = "greenness") %>%
  mutate(
    treated_prev1Y = ifelse(Year %in% c(2022, 2019, 2018, 2017), lag(treated), ifelse(Year %in% c(2021, 2016), 0, treated)),
    treated_prev2Y = ifelse(Year %in% c(2021, 2019, 2018), lag(treated, n = 2), ifelse(Year %in% c(2022, 2017, 2016), 0, treated)),
    post_treatment = ifelse(grepl("preSpray", post_treatment), "0", "1"),
    ATT = as.numeric(treated) * as.numeric(post_treatment),
    ATT_prev1Y = as.numeric(treated_prev1Y) * as.numeric(post_treatment),
    ATT_prev2Y = as.numeric(treated_prev2Y) * as.numeric(post_treatment)
  )

# Extract unique years from panel_data$Year
unique_years <- unique(panel_data$Year)

# Create dummy variables for each unique year and add them to 'panel_data'
year_dummies <- sapply(unique_years, function(year) as.numeric(panel_data$Year == year))
colnames(year_dummies) <- paste("Year", unique_years, sep = "")
panel_data <- cbind(panel_data, year_dummies)

# Organize the column order
panel_data <- panel_data[, c("Pixel_ID", "Year", "greenness",
                             "treated", "post_treatment", "ATT",
                             "treated_prev1Y", "ATT_prev1Y",
                             "treated_prev2Y", "ATT_prev2Y",
                             "Year2016", "Year2017", "Year2018","Year2019","Year2021","Year2022",
                             "precipitation_sum", "aspect", "elevation", "slope", 
                             "latitude", "longitude")]

write.csv(panel_data, "Results/panel_data.csv", row.names = FALSE)

##################
# Panel Model
##################

#Simple DID model (reference year is 2016)
did <- feols(greenness ~ ATT + Year2017 + Year2018 + Year2019 + Year2021 + Year2022 +
               ATT:Year2017 + ATT:Year2018 + ATT:Year2019 + ATT:Year2021 + ATT:Year2022 |
               aspect + slope + elevation,
             data = panel_data,
             cluster = ~Pixel_ID)
msummary(did, stars = c('*' = .1, '**' = .05, '***' = .01))

#DID model with added precipitation (reference year is 2016)
did_precip <- feols(greenness ~ ATT + Year2017 + Year2018 + Year2019 + Year2021 + Year2022 +
                      ATT:Year2017 + ATT:Year2018 + ATT:Year2019 + ATT:Year2021 + ATT:Year2022 +
                      ATT:Year2017:precipitation_sum + ATT:Year2018:precipitation_sum +
                      ATT:Year2019:precipitation_sum + ATT:Year2021:precipitation_sum + ATT:Year2022:precipitation_sum |
                      aspect + slope + elevation,
                    data = panel_data,
                    cluster = ~Pixel_ID)
msummary(did_precip, stars = c('*' = .1, '**' = .05, '***' = .01))