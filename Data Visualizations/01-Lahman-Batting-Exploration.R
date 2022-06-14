# Load packages ----
library(tidyverse)
library(Lahman)

# Initialize and preview the batting dataset ----
Batting <- as_tibble(Batting)

dim(Batting)
class(Batting)

head(Batting)
tail(Batting)

# Preview variables
colnames(Batting)

# Summarize data ----
summary(Batting$yearID)

table(Batting$lgID)

# Check if any values are 'NA'; if false, the values are not NA
any(is.na(Batting$lgID))

# Create a new table with only AL/NL players (%in% is an alternative to -> lgID == 'AL' | lgID == 'NL')
mlb_batting <- filter(Batting, lgID %in% c('AL', 'NL'))

sel_batting <- dplyr::select(Batting, yearID, lgID, G, AB, R, H, HR, BB, SO)

arrange(Batting, desc(HR))

# Create a table with these values (basic summarization)
summarize(Batting, max(stint), median(AB))

# Mutate the table to include a col. for batting average
new_batting <- mutate(Batting, batting_avg = H / AB)

# Using the pipe operator (%>%) ----
Batting %>%
  filter(lgID %in% c('AL','NL'), AB > 300) %>%
  mutate(batting_avg = H/AB) %>%
  arrange(desc(batting_avg)) %>%
  dplyr::select(playerID, yearID, batting_avg) %>%
  slice(c(1,2,5,42,183)) # Display rows 1, 2, 5...

# Group by year
Batting %>%
  filter(lgID %in% c('AL','NL'), AB > 300) %>%
  group_by(yearID, playerID) %>%
  summarize(hr = sum(HR), so = sum(SO), bb = sum(BB), .groups = 'drop') %>% # .groups = 'drop' removes grouping
  arrange(desc(hr)) %>%
  slice(1:5)

# Create year batting summary ----
year_batting_summary <- Batting %>% 
  filter(lgID %in% c('AL', 'NL')) %>%
  group_by(yearID) %>%
  summarize(total_hits = sum(H, na.rm = TRUE),
            total_hrs = sum(HR, na.rm = TRUE),
            total_ks = sum(SO, na.rm = TRUE),
            total_walks = sum(BB, na.rm = TRUE),
            total_abs = sum(AB, na.rm = TRUE)) %>% # Remove NA values 
  mutate(batting_avg = total_hits / total_abs)

year_batting_summary %>%
  select(yearID, batting_avg) %>%
  rename(Year = yearID, `Batting AVG` = batting_avg) %>%
  arrange(desc(`Batting AVG`)) %>%
  slice(c(1:3, (n()-2):n())) # Last three rows

# Summary for visualization
year_batting_summary <- Batting %>%
  filter(lgID %in% c("AL", "NL")) %>%
  group_by(yearID) %>%
  summarize_at(vars(H, HR, SO, BB, AB),
               sum, na.rm = TRUE) %>%
  mutate(batting_avg = H / AB)
