library(sf)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(Matching)

# Define folder and file names
local_folder <- "/usr4/ge501/kbratley/saguaro/Data/wPreviousTreatedANDBufferRemoved"
files <- c("2022_variables.shp", "2021_variables.shp", "2019_variables.shp", 
           "2018_variables.shp", "2017_variables.shp", "2016_variables.shp")

# Organize Data
datasets <- list()

# Read and process each file
for (i in 1:length(files)) {
  print(paste("Reading file:", files[i]))
  combined_data <- st_read(file.path(local_folder, files[i]))
  
  # Add a column for the year, extracted from the file name
  combined_data$Year <- as.numeric(str_extract(files[i], "\\d+"))
  
  # Store data in the list
  datasets[[i]] <- combined_data
  cat("Processed file", i, "of", length(files), "\n")
}

# Combine all datasets into a single data frame
combined_data <- do.call(rbind, datasets)

# Create a unique pixel ID based on lat and lon
combined_data <- combined_data %>%
  group_by(latitude, longitude) %>%
  mutate(Pixel_ID = group_indices())

# Filter out rows with less than 6 corresponding rows for each 'Pixel_ID'
pixel_counts <- combined_data %>%
  group_by(Pixel_ID) %>%
  summarise(row_count = n())

pixels_to_remove <- pixel_counts %>%
  filter(row_count < 6) %>%
  pull(Pixel_ID)

# Ensure that Pixel_ID's are sequential and have no missing #'s
combined_data <- combined_data %>%
  arrange(Pixel_ID)

# Calculate the number of groups and create a sequence of numbers repeating every 6 rows
num_rows <- nrow(combined_data)
num_groups <- ceiling(num_rows / 6)
new_id_sequence <- rep(1:num_groups, each = 6)
new_id_sequence <- rep(new_id_sequence, length.out = num_rows)  # Repeat the sequence to cover all rows

# Add the new_id_sequence as the Pixel_ID column to combined_data
combined_data$New_ID <- new_id_sequence

##############
# Matching
##############

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
d.in <- sf::st_set_geometry(d.in, NULL) # Remove the geometry column

cov <- c('elevation','slope','aspect')
m <- Match(d.in$preSprayGr, d.in$treated, d.in[,cov], caliper=0.5, replace = FALSE)
# print(summary(m))

# Retrieving matched dataset
d.m <- rbind(d.in[m$index.treated,],d.in[m$index.control,])

# Plot the results
# Creating a new categorical treatment variable for mapping
d.m$group <- ifelse(d.m$treated, 'treatment units', 'matched controls')

plot <- ggplot(d.m) +
  geom_point(aes(x = longitude, y = latitude, color = group), alpha = 0.5, size = 0.25) +
  coord_fixed() +
  ggtitle("Matched vs. Treatment Units") +
  theme(legend.text = element_text(size = 16)) +
  theme(legend.title = element_text(size = 16))
print(plot)

################
# Exporting
################
# Exporting matched dataset
if (!file.exists("Results")) {
  dir.create("Results")
}
export_file_name <- "Results/matching_results_combined.shp"
st_write(st_as_sf(d.m, coords = c("longitude", "latitude")), export_file_name)

# Exporting dataset to environment
assign("combined_dataset", d.m)

# Exporting the plot
plot_file_name <- "Results/matched_units_plot.png"
ggsave(plot_file_name, plot, width = 10, height = 8, units = "in", dpi = 500)
print(paste("Saved plot:", plot_file_name))