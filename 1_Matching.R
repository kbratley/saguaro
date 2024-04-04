library(sf)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(Matching)

# Define function to generate URLs
wf <- function(x) paste('https://raw.github.com/kbratley/saguaro/main/', x, sep='')

# Define folder and file names
files <- c("Data/wPreviousTreatedANDBufferRemoved/2022_variables.csv", "Data/wPreviousTreatedANDBufferRemoved/2021_variables.csv", 
           "Data/wPreviousTreatedANDBufferRemoved/2019_variables.csv", "Data/wPreviousTreatedANDBufferRemoved/2018_variables.csv", 
           "Data/wPreviousTreatedANDBufferRemoved/2017_variables.csv", "Data/wPreviousTreatedANDBufferRemoved/2016_variables.csv")

##################
# Organize Data
##################

datasets <- list()

# Read and process each file
for (i in 1:length(files)) {
  print(paste("Reading file:", files[i]))
  url <- wf(files[i])
  combined_data <- read.csv(url)
  combined_data$Year <- as.numeric(str_extract(files[i], "\\d+"))
  datasets[[i]] <- combined_data
  cat("Processed file", i, "of", length(files), "\n")
}

# Combine all datasets into a single data frame
combined_data <- do.call(rbind, datasets)

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

#NEEDS WORK
#File is too big to save to github
# combined_data2 <- combined_data[, c("Pixel_ID", "Year", "treated", 
#                              "preSprayGreenness", "postSprayGreenness", 
#                              "aspect", "elevation", "slope", 
#                              "latitude", "longitude")]
# write.csv(combined_data2, file = "Results/combined_data.csv", row.names = FALSE)

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
# d.in <- sf::st_set_geometry(d.in, NULL) # Remove the geometry column

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

##################
# Exporting
##################
# Exporting matched dataset
if (!file.exists("Results")) {
  dir.create("Results")
}
st_write(st_as_sf(d.m, coords = c("longitude", "latitude")), "Results/matching_results.shp")
write.csv(d.m, file = "Results/matching_results.csv", row.names = FALSE)

# Exporting dataset to environment
assign("combined_dataset", d.m)

# Exporting the plot
plot_file_name <- "Results/matched_units_plot.png"
ggsave(plot_file_name, plot, width = 10, height = 8, units = "in", dpi = 500)
print(paste("Saved plot:", plot_file_name))