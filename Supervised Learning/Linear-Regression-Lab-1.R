# Linear Regression Lab 1


# Load the dataset --------------------------------------------------------

library("tidyverse")
nba_data_2022 <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/intro_r/nba_2022_player_stats.csv")

nba_data_2022 <- nba_data_2022 %>%
  # Summarize player stats across multiple teams they played for:
  group_by(player) %>%
  summarize(age = first(age),
            position = first(position),
            games = sum(games, na.rm = TRUE),
            minutes_played = sum(minutes_played, na.rm = TRUE),
            field_goals = sum(field_goals, na.rm = TRUE),
            field_goal_attempts = sum(field_goal_attempts, na.rm = TRUE),
            three_pointers = sum(three_pointers, na.rm = TRUE),
            three_point_attempts = sum(three_point_attempts, na.rm = TRUE),
            free_throws = sum(free_throws, na.rm = TRUE),
            free_throw_attempts = sum(free_throw_attempts, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(field_goal_percentage = field_goals / field_goal_attempts,
         three_point_percentage = three_pointers / three_point_attempts,
         free_throw_percentage = free_throws / free_throw_attempts,
         min_per_game = minutes_played / games) %>%
  # Remove rows with missing missing values
  drop_na() %>%
  filter(minutes_played > 250)


# EDA on the dataset ------------------------------------------------------

# Scatterplots for each exp. variable
nba_data_2022 %>%
  ggplot(aes(x = min_per_game,
             y = field_goal_percentage)) +
  geom_point(alpha = 0.5) +
  theme_bw()

nba_data_2022 %>%
  ggplot(aes(x = min_per_game,
             y = free_throw_percentage)) +
  geom_point(alpha = 0.5) +
  theme_bw()

nba_data_2022 %>%
  ggplot(aes(x = min_per_game,
             y = three_point_percentage)) +
  geom_point(alpha = 0.5) +
  theme_bw()

nba_data_2022 %>%
  ggplot(aes(x = min_per_game,
             y = age)) +
  geom_point(alpha = 0.5) +
  theme_bw()

# Dist. of minutes played by pos.
nba_data_2022 %>%
  ggplot(aes(x=min_per_game)) +
  geom_histogram() +
  theme_bw() +
  facet_wrap(~ position, 
             ncol = 2,
             scales = "free_y") # Break into subplots with free y-axes

# Fit a linear model: FG % ------------------------------------------------------

init_nba_lm <- lm(min_per_game ~ field_goal_percentage, 
                  data = nba_data_2022)

library(ggfortify)
autoplot(init_nba_lm) +
  theme_bw()


# Assess the model summary

summary(init_nba_lm)

nba_data_2022 %>%
  mutate(init_preds = predict(init_nba_lm)) %>%
  ggplot(aes(x = init_preds, y = min_per_game)) +
  geom_point(alpha = 0.75) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "red") +
  theme_bw() +
  labs(x = "Predictions", y = "Observed minutes / game")

# Fit a linear model: FT % ------------------------------------------------------

init_nba_lm <- lm(min_per_game ~ free_throw_percentage, 
                  data = nba_data_2022)

library(ggfortify)
autoplot(init_nba_lm) +
  theme_bw()


# Assess the model summary

summary(init_nba_lm)

nba_data_2022 %>%
  mutate(init_preds = predict(init_nba_lm)) %>%
  ggplot(aes(x = init_preds, y = min_per_game)) +
  geom_point(alpha = 0.75) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "red") +
  theme_bw() +
  labs(x = "Predictions", y = "Observed minutes / game")

# Fit a linear model: 3PT % ------------------------------------------------------

init_nba_lm <- lm(min_per_game ~ three_point_percentage, 
                  data = nba_data_2022)

library(ggfortify)
autoplot(init_nba_lm) +
  theme_bw()


# Assess the model summary

summary(init_nba_lm)

nba_data_2022 %>%
  mutate(init_preds = predict(init_nba_lm)) %>%
  ggplot(aes(x = init_preds, y = min_per_game)) +
  geom_point(alpha = 0.75) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "red") +
  theme_bw() +
  labs(x = "Predictions", y = "Observed minutes / game")

# Fit a linear model: AGE ------------------------------------------------------

init_nba_lm <- lm(min_per_game ~ age, 
                  data = nba_data_2022)

library(ggfortify)
autoplot(init_nba_lm) +
  theme_bw()


# Assess the model summary

summary(init_nba_lm)

nba_data_2022 %>%
  mutate(init_preds = predict(init_nba_lm)) %>%
  ggplot(aes(x = init_preds, y = min_per_game)) +
  geom_point(alpha = 0.75) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "red") +
  theme_bw() +
  labs(x = "Predictions", y = "Observed minutes / game")


# Multiple covariates -----------------------------------------------------

multi_nba_lm <- lm(min_per_game ~ field_goal_percentage + three_point_percentage,
                   data = nba_data_2022)

# Variance inflation factor (VIF)
library(car)
vif(multi_nba_lm)

# Assess the model summary

summary(multi_nba_lm)

autoplot(multi_nba_lm) + theme_bw()
