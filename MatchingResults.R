# MATCHING WITH REPLACEMENT 
library(knitr)
library(kableExtra)


wf <- function(x) paste('https://raw.githubusercontent.com/kbratley/saguaro/main/', x, sep='')

# Function to combine summary tables
combine_summaries <- function(summary_list) {
  # Combine summary tables
  combined_summary <- do.call(rbind, summary_list)
  return(combined_summary)
}

summary_2016 <- list(
  Estimate = -0.0084407,
  AI_SE = 0.00070634,
  T_stat = -11.95,
  p_val = "< 2.22e-16",
  Original_obs = 192497,
  Original_treated_obs = 1138,
  Matched_obs = 1138,
  Matched_obs_unweighted = 1510,
  Caliper_SDs = c(0.5, 0.5, 0.5),
  Obs_dropped = 0
)

summary_2017 <- list(
  Estimate = -0.019028,
  AI_SE = 0.00054776,
  T_stat = -34.737,
  p_val = "< 2.22e-16",
  Original_obs = 192896,
  Original_treated_obs = 2536,
  Matched_obs = 2535,
  Matched_obs_unweighted = 2895,
  Caliper_SDs = c(0.5, 0.5, 0.5),
  Obs_dropped = 1
)

summary_2018 <- list(
  Estimate = -0.0067385,
  AI_SE = 0.00031008,
  T_stat = -21.731,
  p_val = "< 2.22e-16",
  Original_obs = 193248,
  Original_treated_obs = 4469,
  Matched_obs = 4469,
  Matched_obs_unweighted = 4989,
  Caliper_SDs = c(0.5, 0.5, 0.5),
  Obs_dropped = 0
)

summary_2019 <- list(
  Estimate = -0.0049007,
  AI_SE = 0.00037962,
  T_stat = -12.909,
  p_val = "< 2.22e-16",
  Original_obs = 176732,
  Original_treated_obs = 2909,
  Matched_obs = 2909,
  Matched_obs_unweighted = 3309,
  Caliper_SDs = c(0.5, 0.5, 0.5),
  Obs_dropped = 0
)

summary_2021 <- list(
  Estimate = 0.011529,
  AI_SE = 0.00081308,
  T_stat = 14.18,
  p_val = "< 2.22e-16",
  Original_obs = 192735,
  Original_treated_obs = 2155,
  Matched_obs = 2155,
  Matched_obs_unweighted = 2429,
  Caliper_SDs = c(0.5, 0.5, 0.5),
  Obs_dropped = 0
)

summary_2022 <- list(
  Estimate = 0.017885,
  AI_SE = 0.00077222,
  T_stat = 23.161,
  p_val = "< 2.22e-16",
  Original_obs = 191855,
  Original_treated_obs = 2170,
  Matched_obs = 2170,
  Matched_obs_unweighted = 2578,
  Caliper_SDs = c(0.5, 0.5, 0.5),
  Obs_dropped = 0
)

# Create a list of summaries
summary_list <- list(summary_2016, summary_2017, summary_2018, summary_2019, summary_2021, summary_2022)

# Call the function to combine summaries
combined_summary_withReplacement <- combine_summaries(summary_list)
transposed_summary <- t(combined_summary_withReplacement)
col_names <- c("2016", "2017", "2018", "2019", "2021", "2022")
colnames(transposed_summary) <- col_names

# Export the data frame to a CSV file
file_path <- "Results/matchingResults_WithReplacement.csv"
write.csv(transposed_summary, file = file_path, row.names = TRUE)

# Create a table using kable\
table <- kable(transposed_summary) %>%
  kable_styling(
    latex_options = "striped",
    full_width = FALSE,
    bootstrap_options = c("striped", "condensed", "bordered")
  )
kable(head(transposed_summary, 10), booktabs = TRUE) %>%
  kable_styling(font_size = 12) %>%
  add_header_above(c("Matching with Replacement" = 7))
  

# MATCHING *WITHOUT* REPLACEMENT 
summary_2016 <- list(
  Estimate = -0.0084676,
  AI_SE = 0.00069636,
  T_stat = -12.16,
  p_val = "< 2.22e-16",
  Original_obs = 192497,
  Original_treated_obs = 1138,
  Matched_obs = 1138,
  Matched_obs_unweighted = 1138,
  Caliper_SDs = c(0.5, 0.5, 0.5),
  Obs_dropped = 0
)

summary_2017 <- list(
  Estimate = -0.018871,
  AI_SE = 0.00050682,
  T_stat = -37.234,
  p_val = "< 2.22e-16",
  Original_obs = 192896,
  Original_treated_obs = 2536,
  Matched_obs = 2535,
  Matched_obs_unweighted = 2535,
  Caliper_SDs = c(0.5, 0.5, 0.5),
  Obs_dropped = 1
)

summary_2018 <- list(
  Estimate = -0.0067385,
  AI_SE = 0.0002838,
  T_stat = -23.178,
  p_val = "< 2.22e-16",
  Original_obs = 193248,
  Original_treated_obs = 4469,
  Matched_obs = 4469,
  Matched_obs_unweighted = 4469,
  Caliper_SDs = c(0.5, 0.5, 0.5),
  Obs_dropped = 0
)

summary_2019 <- list(
  Estimate = -0.004831,
  AI_SE = 0.00037962,
  T_stat = -13.618,
  p_val = "< 2.22e-16",
  Original_obs = 176732,
  Original_treated_obs = 2909,
  Matched_obs = 2909,
  Matched_obs_unweighted = 2909,
  Caliper_SDs = c(0.5, 0.5, 0.5),
  Obs_dropped = 0
)

summary_2021 <- list(
  Estimate = 0.012076,
  AI_SE = 0.00078956,
  T_stat = 15.294,
  p_val = "< 2.22e-16",
  Original_obs = 192735,
  Original_treated_obs = 2155,
  Matched_obs = 2155,
  Matched_obs_unweighted = 2155,
  Caliper_SDs = c(0.5, 0.5, 0.5),
  Obs_dropped = 0
)

summary_2022 <- list(
  Estimate = 0.018287,
  AI_SE = 0.00074432,
  T_stat = 24.569,
  p_val = "< 2.22e-16",
  Original_obs = 191855,
  Original_treated_obs = 2170,
  Matched_obs = 2170,
  Matched_obs_unweighted = 2170,
  Caliper_SDs = c(0.5, 0.5, 0.5),
  Obs_dropped = 0
)

# Create a list of summaries
summary_list <- list(summary_2016, summary_2017, summary_2018, summary_2019, summary_2021, summary_2022)

# Call the function to combine summaries
combined_summary_withoutReplacement <- combine_summaries(summary_list)
transposed_summary <- t(combined_summary_withoutReplacement)
col_names <- c("2016", "2017", "2018", "2019", "2021", "2022")
colnames(transposed_summary) <- col_names

# Export the data frame to a CSV file
file_path <- "Results/matchingResults_WithoutReplacement.csv"
write.csv(transposed_summary, file = file_path, row.names = TRUE)

# Create a table using kable\
table <- kable(transposed_summary) %>%
  kable_styling(
    latex_options = "striped",
    full_width = FALSE,
    bootstrap_options = c("striped", "condensed", "bordered")
  )
kable(head(transposed_summary, 10), booktabs = TRUE) %>%
  kable_styling(font_size = 12) %>%
  add_header_above(c("Matching *without* Replacement" = 7))
