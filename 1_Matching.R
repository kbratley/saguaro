library(ggplot2)
library(tidyverse)
library(Matching)
library(dplyr)
library(sf)

# # Setting up the environment and calling files
# wf <- function(x) paste('https://raw.githubusercontent.com/kbratley/saguaro/main/', x, sep='')
# 
# # List of file names
# files <- c("Data/wPreviousTreatedANDBufferRemoved/2016_variables.shp", "Data/wPreviousTreatedANDBufferRemoved/2017_variables.shp", "Data/wPreviousTreatedANDBufferRemoved/2018_variables.shp",
#            "Data/wPreviousTreatedANDBufferRemoved/2019_variables.shp", "Data/wPreviousTreatedANDBufferRemoved/2021_variables.shp", "Data/wPreviousTreatedANDBufferRemoved/2022_variables.shp")

# ## PART 1 ##
# # Loop over files to run matching for each dataset
# for (file in files) {
#   # Load data
#   file_url <- wf(file)
#   d <- st_read(file_url)
# }


# Set the local path to your folder
local_folder <- "/users/kelseebratley/saguaro/Data/wPreviousTreatedANDBufferRemoved/"

# List of file names
files <- c("2022_variables.shp", "2021_variables.shp", "2019_variables.shp", "2018_variables.shp", "2017_variables.shp", "2016_variables.shp")

## PART 1 ##
# Loop over files to run matching for each dataset
for (file in files) {
  # Construct the local file path
  file_path <- file.path(local_folder, file)
  # Load data
  d <- st_read(file_path)
  
  # Visualizing treated vs. untreated
  d$cat <- ifelse(d$treated==0, 'untreated',
                  ifelse(d$treated==1, 'treated', NA))

  # boolean indexing:
  # d <- d[!is.na(d$cat),]
  # table(d$cat)  # counts pixels for each group
  na_indices <- is.na(d$cat)

  # Subset the data frame only if there are no NA values
  if (any(na_indices)) {
    d <- d[!na_indices,]
  }

  # # Treated vs Not
  # ggplot(d) +
  #   geom_point(aes(x=longitude,y=latitude,color=treated), alpha=0.5, size=0.25) +
  #   coord_fixed() 
  # 
  # # Plot Greenness Difference
  # ggplot(d) +
  #   geom_point(aes(x=longitude,y=latitude,color=greennessC), alpha=0.5, size=0.25) +
  #   scale_color_gradient(low='white',high='forestgreen') +
  #   coord_fixed() +
  #   ggtitle('Greenness Change')
  # 
  # # Plot Slope
  # ggplot(d) +
  #   geom_point(aes(x=longitude,y=latitude,color=slope), alpha=0.5, size=0.25) +
  #   scale_color_gradient(low='white',high='black') +
  #   coord_fixed() +
  #   ggtitle('Slope')
  # 
  # # Plot Elevation
  # ggplot(d) +
  #   geom_point(aes(x=longitude,y=latitude,color=elevation), alpha=0.5, size=0.25) +
  #   scale_color_gradient(low='white',high='black') +
  #   coord_fixed() +
  #   ggtitle('Elevation')
  # 
  # # Plot aspect
  # ggplot(d) +
  #   geom_point(aes(x=longitude,y=latitude,color=aspect), alpha=0.5, size=0.25) +
  #   scale_color_gradient(low='white',high='black') +
  #   coord_fixed() +
  #   ggtitle('Aspect')

  
  # Matching
  d.tr <- d[d$cat=='treated',]
  d.ct <- d[d$cat=='untreated',]
  d.tr$tr <- 1
  d.ct$tr <- 0
  d.in <- rbind(d.tr, d.ct)
  # Remove the geometry column
  d.in <- sf::st_set_geometry(d.in, NULL)

  cov <- c('elevation','slope','aspect')
  m <- Match(d.in$preSprayGr, d.in$tr, d.in[,cov], caliper=0.5, replace = FALSE)

  # Print summary of the matching
  print(summary(m))

  # Retrieving matched dataset
  d.m <- rbind(d.in[m$index.treated,],d.in[m$index.control,])

  # Creating a new categorical treatment variable for mapping
  d.m$group <- ifelse(d.m$tr, 'treatment units', 'matched controls')

  # Exporting 'm' variable for each year
  if (!file.exists("Results")) {
    dir.create("Results")
  }
  year_label <- str_extract(file, "\\d{4}")
  export_file_name <- paste0("Results/matching_results_", year_label, ".shp")
  st_write(st_as_sf(d.m, coords = c("longitude", "latitude")), export_file_name)

  # Exporting dataset to environment
  assign(paste0("dataset_", year_label), d.m)
}

## PART 2 ##
# Aggregate matched results to serve as a 'mask' for filtering panel model data
dataset_variables <- c("treated", "group", "latitude", "longitude", ".geo")

# Initialize the final combined dataset
final_dataset <- NULL

# Loop over years
for (year_label in c("2016", "2017", "2018", "2019", "2021", "2022")) {
  # Load each dataset from the environment
  dataset <- get(paste0("dataset_", year_label))

  # Select the relevant columns
  dataset <- dataset[, dataset_variables, drop = FALSE]

  # Combine datasets
  if (is.null(final_dataset)) {
    final_dataset <- dataset
  } else {
    final_dataset <- bind_rows(final_dataset, dataset)
  }
}

# Group by '.geo' and summarize 'treated' column
final_dataset <- final_dataset %>%
  group_by(.geo) %>%
  summarise(treated = ifelse(any(treated == 1), 0, 1),
            group = first(group),
            latitude = first(latitude),
            longitude = first(longitude))

# Print the final combined dataset
head(final_dataset)

# Save the final combined dataset as CSV
write.csv(final_dataset, "Results/final_combined_dataset.csv", row.names = FALSE)

# Save the final combined dataset as shapefile
export_file_name <- paste0("Results/final_combined_dataset.shp")
st_write(st_as_sf(final_dataset, coords = c("longitude", "latitude")), export_file_name)

# Function to save and plot matched units
plot_matched_units <- function(dataset, year) {
  # Plot
  plot <- ggplot(dataset) +
    geom_point(aes(x = longitude, y = latitude, color = group), alpha = 0.5, size = 0.25) +
    coord_fixed() +
    ggtitle(paste(year, "- Matched vs. Treatment Units")) +
    theme(legend.text = element_text(size = 16)) +
    theme(legend.title = element_text(size = 16))
  
  # Save plot
  plot_file_name <- paste0("Results/matched_plots/", year, "_matched_units_plot.png")
  ggsave(plot_file_name, plot, width = 8, height = 6, units = "in", dpi = 300)
  print(paste("Saved plot:", plot_file_name))
  
  return(plot)
}

plot_matched_units(dataset_2016, "2016")
plot_matched_units(dataset_2017, "2017")
plot_matched_units(dataset_2018, "2018")
plot_matched_units(dataset_2019, "2019")
plot_matched_units(dataset_2021, "2021")
plot_matched_units(dataset_2022, "2022")
plot_matched_units(final_dataset, "All")