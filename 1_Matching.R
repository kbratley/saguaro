library(sf)
library(stringr)
library(dplyr)
library(ggplot2)
library(Matching)
library(raster)

# Define function to generate URLs
wf <- function(x) paste('https://raw.github.com/kbratley/saguaro/main/', x, sep='')

# List of file names
files <- paste0("Data/", c(2016:2019, 2021:2022), "_variables.csv")

# List of mask file names
mask_files <- paste0("Data/Mask_", c(2016:2019, 2021:2022), ".tif")

##################
# Organize Data
##################

# # Define a function to plot masked data
# plot_masked_data <- function(data, year) {
#   print(ggplot() +
#           geom_sf(data = data, color = "blue") +
#           labs(title = paste("Masked Data for Year", year)) +
#           theme_minimal())
# }

# Organize Data
datasets <- list()

# Read and process each file
for (i in 1:length(files)) {
  url <- wf(files[i]) 
  combined_data <- read.csv(url)
  year <- as.numeric(str_extract(files[i], "\\d+"))  # Extract year from filename
  mask <- raster(mask_files[i])
  combined_data <- st_as_sf(combined_data, coords = c("longitude", "latitude"), crs = st_crs(mask))  # Convert csv to sf object
  combined_data$mask_value <- extract(mask, combined_data)  # Extract values from raster to points
  combined_data <- combined_data[combined_data$mask_value == 1, ]  # Filter out rows where mask doesn't equal 1
  combined_data$Year <- year  # Add year information
  
  # Create a unique pixel ID based on lat and lon
  combined_data <- combined_data %>%
    group_by(geometry) %>%
    mutate(Pixel_ID = cur_group_id())
  
  datasets[[i]] <- combined_data
  # plot_masked_data(combined_data, year)
  cat("Processed file", i, "of", length(files), "\n")
}

# Combine all datasets into a single data frame
combined_data <- do.call(rbind, datasets)

# Sort by Pixel_ID in ascending order
combined_data <- combined_data %>%
  arrange(Pixel_ID)

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
num_rows <- nrow(combined_data)
num_groups <- ceiling(num_rows / 6) #calculate # of groups
new_id_sequence <- rep(1:num_groups, each = 6) #create sequence of #'s repeating every 6 rows
new_id_sequence <- rep(new_id_sequence, length.out = num_rows)  # Repeat the sequence to cover all rows

# Remove the old Pixel_ID column and update it to reflect the new_id_sequence
combined_data <- combined_data[, !names(combined_data) %in% "Pixel_ID"]
combined_data$Pixel_ID <- new_id_sequence

# Extract longitude and latitude from geometry column
combined_sf <- st_as_sf(combined_data, wkt = "geometry")
combined_data$longitude <- st_coordinates(combined_sf)[, 1]
combined_data$latitude <- st_coordinates(combined_sf)[, 2]

##################
# Matching
##################

# Visualizing treated vs. untreated
combined_data$cat <- ifelse(combined_data$treated == 0, 'untreated',
                            ifelse(combined_data$treated == 1, 'treated', NA))

# Subset the data frame only if there are no NA values
na_indices <- is.na(combined_data$cat)
if (any(na_indices)) {
  combined_data <- combined_data[!na_indices,]
}

d.tr <- combined_data[combined_data$cat == 'treated',]
d.ct <- combined_data[combined_data$cat == 'untreated',]
d.in <- rbind(d.tr, d.ct)

# Convert elevation column to numeric
d.in$elevation <- as.numeric(d.in$elevation)

cov <- c('elevation','slope','aspect','precipitation_sum')

# Extract necessary columns as a data frame
matching_data <- d.in[, c("preSprayGreenness", "treated", cov)]
matching_data <- sf::st_set_geometry(matching_data, NULL) # Remove the geometry column

# Perform matching
m <- Match(matching_data$preSprayGreenness, matching_data$treated, as.matrix(matching_data[, cov]), caliper = 0.5, replace = FALSE)
# print(summary(m))

# Retrieving matched dataset
d.m <- rbind(d.in[m$index.treated,],d.in[m$index.control,])

# Plot the results
# Creating a new categorical treatment variable for mapping
d.m$group <- ifelse(d.m$treated, 'treatment units', 'matched controls')

# Create the plot
plot <- ggplot(d.m) +
  geom_point(aes(x = longitude, y = latitude, color = group), alpha = 0.5, size = 0.25) +
  coord_fixed() +
  ggtitle("Matched vs. Treatment Units") +
  theme(legend.text = element_text(size = 16)) +
  theme(legend.title = element_text(size = 16))
print(plot)

##################
# Exporting
##################
d.m <- ungroup(d.m)
st_write(st_as_sf(d.m, coords = c("longitude", "latitude")), "Results/matching_results.shp")
write.csv(d.m, file = "Results/matching_results.csv", row.names = FALSE)

# Exporting dataset to environment
assign("combined_dataset", d.m)

# Exporting the plot
plot_file_name <- "Results/matched_units_plot.png"
ggsave(plot_file_name, plot, width = 10, height = 8, units = "in", dpi = 500)
print(paste("Saved plot:", plot_file_name))
