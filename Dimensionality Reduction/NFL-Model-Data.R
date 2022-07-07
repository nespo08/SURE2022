# PURPOSE: Load and save NFL model data


# Load the data -----------------------------------------------------------

library(tidyverse)
nfl_teams_data <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/regression_examples/nfl_team_season_summary.csv")


# Preprocess the dataset --------------------------------------------------

nfl_model_data <- nfl_teams_data %>%
  mutate(score_diff = points_scored - points_allowed) %>%
  # Only use rows with air yards
  filter(season >= 2006) %>%
  dplyr::select(-wins, -losses, -ties, -points_scored, -points_allowed, -season, -team)
nfl_model_data


# Save the NFL model data -------------------------------------------------

# CSV
write_csv(nfl_model_data,
          "Data/nfl_model_data.csv")

# RDS object
write_rds(nfl_model_data, 
          "Data/nfl_model_data.rds")