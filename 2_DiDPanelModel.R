library(dplyr)
library(stringr)
library(sf)
library(tidyverse)
library(modelsummary)
library(fixest)
library(ggplot2)

# Setting up the environment and calling files
wf <- function(x) paste('https://raw.githubusercontent.com/kbratley/saguaro/main/', x, sep='')

# List of file names
files <- paste0("Data/", c(2016:2019, 2021:2022), "_variables.csv")

# Load the final matching shapefile for clipping
clip_shapefile <- st_read("Results/matching_results.shp")

##################
# Organize Data
##################

# Function to extract year from file names
get_year <- function(file_path) {
  basename(file_path) %>% 
    str_extract("\\d{4}") %>%  # Extract four consecutive digits (year)
    as.integer()  # Convert to integer
}

# Add 'Year' column to each dataset
combined_data <- files %>%
  map_df(~ read.csv(.x) %>% mutate(Year = get_year(.x)))

# Convert to sf object
combined_sf <- st_as_sf(combined_data, coords = c("longitude", "latitude"), crs = 4326)

# Transform combined_sf to match the CRS of clip_shapefile
combined_sf <- st_transform(combined_sf, st_crs(clip_shapefile))

# Add .y to the end of every column (excluding geometry column) to help with filtering later on
clip_shapefile <- clip_shapefile %>%
  rename_with(~ if_else(. == "geometry", ., paste0(., ".y")), -geometry)

# Perform a spatial join
selected_points <- st_join(combined_sf, clip_shapefile) %>%
  filter(mask_vl.y == 1) %>%
  dplyr::select(-ends_with(".y"))  # Exclude columns with .y suffix

      ggplot() +
        geom_sf(data = selected_points, color = "blue", fill = NA) +
        # geom_sf(data = clip_shapefile, color = "red", fill = NA) +
        theme_minimal()

# Create a unique pixel ID based on lat and lon
selected_points <- selected_points %>%
  group_by(geometry) %>%
  mutate(Pixel_ID = cur_group_id())

# Filter out rows with less than 6 corresponding rows for each 'Pixel_ID'
pixel_counts <- selected_points %>%
  group_by(Pixel_ID) %>%
  summarise(row_count = n())

pixels_to_remove <- pixel_counts %>%
  filter(row_count < 6) %>%
  pull(Pixel_ID)

selected_points <- selected_points %>%
  arrange(Pixel_ID)

# Ensure that Pixel_ID is sequential and has no missing #'s
num_rows <- nrow(selected_points)
num_groups <- ceiling(num_rows / 6) #calculate # of groups
new_id_sequence <- rep(1:num_groups, each = 6) #create sequence of #'s repeating every 6 rows
new_id_sequence <- rep(new_id_sequence, length.out = num_rows)  # Repeat the sequence to cover all rows

# Remove the old Pixel_ID column and update it to reflect the new_id_sequence
selected_points <- selected_points[, !names(selected_points) %in% "Pixel_ID"]
selected_points$Pixel_ID <- new_id_sequence

# New dataframe to prevent errors
panel_data <- selected_points

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
                             "geometry")]

# write.csv(panel_data, "Results/panel_data.csv", row.names = FALSE)

# Count yearly overlap areas
count_pixels <- function(data, year) {
  num_pixels_both <- sum(data$treated_prev1Y == 1 & data[[year]] == 1, na.rm = TRUE)
  print(paste("Number of pixels that equal 1 in both treated_prev1Y and ", year, ": ", num_pixels_both))
}

count_pixels(panel_data, "Year2017")
count_pixels(panel_data, "Year2018")
count_pixels(panel_data, "Year2019")
count_pixels(panel_data, "Year2022")

##################
# Panel Model
##################

#Simple DID model (reference year is 2016)
did <- feols(greenness ~ treated + post_treatment + ATT + Year2017 + Year2018 + Year2019 + Year2021 + Year2022 +
               ATT:Year2017 + ATT:Year2018 + ATT:Year2019 + ATT:Year2021 + ATT:Year2022 |
               aspect + slope + elevation,
             data = panel_data,
             cluster = ~Pixel_ID)
msummary(did, stars = c('*' = .1, '**' = .01, '***' = .001))

# Adding in spatial lag to test how the treatment status in the previous year impacts the treatment effect on greenness in subsequent years
did <- feols(greenness ~ treated_prev1Y + post_treatment + ATT_prev1Y + Year2018 + Year2019 + Year2022 +
                ATT_prev1Y:Year2018 + ATT_prev1Y:Year2019 +  ATT_prev1Y:Year2022 |
               aspect + slope + elevation,
             data = panel_data,
             cluster = ~Pixel_ID)
msummary(did, stars = c('*' = .1, '**' = .01, '***' = .001))
