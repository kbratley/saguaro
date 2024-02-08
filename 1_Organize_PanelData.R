library(dplyr)
library(stringr)

# Setting up the environment and calling files
wf <- function (x) paste('https://raw.githubusercontent.com/kbratley/saguaro/main/Data/', x, sep='')

# List of file names
files <- c(wf("2016_covariates.csv"), wf("2017_covariates.csv"), wf("2018_covariates.csv"), 
           wf("2019_covariates.csv"), wf("2021_covariates.csv"), wf("2022_covariates.csv"))


# Initialize an empty data frame to store the combined data
combined_data <- data.frame()

# Loop through each file
for (i in 1:length(files)) {
  # Read the file
  print(paste("Reading file:", files[i]))
  data <- read.csv(files[i])
  
  # Add a column for the year, extracted from the file name
  data$Year <- as.numeric(str_extract(files[i], "\\d+"))
  
  # Combine the data
  combined_data <- rbind(combined_data, data)
  
  # Print progress
  cat("Processed file", i, "of", length(files), "\n")
}

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

# Apply the function to the combined data frame
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

# Organize data match question formats:
# Treated in the current year and previous year 
combined_data$treated_2yrs_consecutively <- ifelse(combined_data$treated == 1 & combined_data$treated_Prev1Y == 1, 1, 0)

# Treated in previous year but not current
combined_data$treated_1yrPrior_notFollowingYr <- ifelse(combined_data$treated == 0 & combined_data$treated_Prev1Y == 1, 1, 0)

# Rename greenness impact variable
colnames(combined_data)[colnames(combined_data) == "greennessImpact"] <- "greennessChange"

# Organize the column order
Panel <- combined_data[, c('Pixel_ID', 'Year', 'greennessChange',
                           'treated', 'treated_1yrPrior_notFollowingYr', 'treated_2yrs_consecutively',
                           'aspect_transform', 'elevation', 'slope', 'preSprayGreenness','postSprayGreenness')]

write.csv(Panel, "Panel", row.names = FALSE)

# Count the occurrences of each year in the 'Year' column - make sure they all match
year_counts <- table(combined_data$Year)

# Print the results
cat("Year counts:\n")
print(year_counts)
