library(ggplot2)
library(tidyverse)
library(Matching)
library(dplyr)
library(sf)

# Setting up the environment and calling files
wf <- function(x) paste('https://raw.githubusercontent.com/kbratley/saguaro/main/', x, sep='')

# List of file names
files <- c("Data/2016_variables.csv", "Data/2017_variables.csv", "Data/2018_variables.csv", 
           "Data/2019_variables.csv", "Data/2021_variables.csv", "Data/2022_variables.csv")

## PART 1 ##
# Loop over files to run matching for each dataset
for (file in files) {
  # Load data
  file_url <- wf(file)
  d <- read.csv(file_url)
  
  # Function to create new column for cosine of aspect with a 45-degree shift
  transform_aspect <- function(data, aspect_col_name) {
    data %>%
      mutate(aspect_transform = cos((!!sym(aspect_col_name)-45)))
  }
  
  # Apply the function to the combined data frame
  d <- transform_aspect(d, "aspect")
  
  head(d)
  # Visualizing treated vs. untreated
  d$cat <- ifelse(d$treated==0, 'untreated', 
                  ifelse(d$treated==1, 'treated', NA))
  # boolean indexing:
  d <- d[!is.na(d$cat),]
  table(d$cat)  # counts pixels for each group
  
  # Matching
  d.tr <- d[d$cat=='treated',]
  d.ct <- d[d$cat=='untreated',]
  d.tr$tr <- 1
  d.ct$tr <- 0
  d.in <- rbind(d.tr, d.ct)
  
  # Running Match
  cov <- c('elevation','slope','aspect', 'preSprayGreenness')
  m <- Match(d.in$greennessDiff, d.in$tr, d.in[,cov], caliper=0.5)
  
  # Retrieving matched dataset
  d.m <- rbind(d.in[m$index.treated,],d.in[m$index.control,])
  
  # Creating a new categorical treatment variable for mapping
  d.m$group <- ifelse(d.m$tr, 'treatment units', 'matched controls')
  print(summary(m))
  
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
# write.csv(final_dataset, "Results/final_combined_dataset.csv", row.names = FALSE)

# Save the final combined dataset as shapefile
# export_file_name <- paste0("Results/final_combined_dataset.shp")
# st_write(st_as_sf(final_dataset, coords = c("longitude", "latitude")), export_file_name)

# plot
plot_matched_units <- function(dataset, year) {
  ggplot(dataset) +
    geom_point(aes(x = longitude, y = latitude, color = group), alpha = 0.5, size = 0.25) +
    coord_fixed() +
    ggtitle(paste(year, "- Matched vs. Treatment Units")) +
    theme(legend.text = element_text(size = 16)) +
    theme(legend.title = element_text(size = 16))
}

plot_matched_units(dataset_2016, "2016")
plot_matched_units(dataset_2017, "2017")
plot_matched_units(dataset_2018, "2018")
plot_matched_units(dataset_2019, "2019")
plot_matched_units(dataset_2021, "2021")
plot_matched_units(dataset_2022, "2022")
plot_matched_units(final_dataset, "All")