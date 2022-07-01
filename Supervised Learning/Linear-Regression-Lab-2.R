# Linear Regression Lab 1


# Load the data -----------------------------------------------------------

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

# Fit a model on the position variable
pos_nba_lm <- lm(min_per_game ~ position, data = nba_data_2022)

# Create a new column with the model's predictions, then plot by position
nba_data_2022 %>%
  mutate(pos_preds = predict(pos_nba_lm)) %>%
  ggplot(aes(x = min_per_game, y = pos_preds)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ position, ncol = 3) +
  theme_bw() +
  labs(x = "Actual minutes / game", 
       y = "Predicted minutes / game")

summary(pos_nba_lm)

# Modify the position variable so that SG is the baseline
nba_data_2022 <- nba_data_2022 %>%
  mutate(position = fct_relevel(position, "SG")) 

# Refit the model
pos_nba_lm <- lm(min_per_game ~ position, data = nba_data_2022)

summary(pos_nba_lm)

# Fit the model with position and FG%
x_pos_nba_lm <- lm(min_per_game ~ position + field_goal_percentage, 
                   data = nba_data_2022)

nba_data_2022 %>%
  mutate(x_pos_preds = predict(x_pos_nba_lm)) %>%
  ggplot(aes(x = field_goal_percentage, 
             y = x_pos_preds,
             color = position)) +
  geom_point(alpha = 0.5) +
  theme_bw() +
  labs(x = "FG%", 
       y = "Predicted MPG")

# Group positions into position groups and refit the model
nba_data_2022 <- nba_data_2022 %>%
  mutate(position_group = fct_collapse(position,
                                       Guard = c("SG", "PG"),
                                       Forward = c("SF", "PF"),
                                       Center = "C")) 

x_pos_group_nba_lm <- lm(min_per_game ~ position_group + field_goal_percentage, 
                         data = nba_data_2022)

nba_data_2022 %>%
  mutate(x_pos_group_preds = predict(x_pos_group_nba_lm)) %>%
  ggplot(aes(x = field_goal_percentage, 
             y = x_pos_group_preds,
             color = position_group)) +
  geom_point(alpha = 0.5) +
  theme_bw() +
  labs(x = "FG%", 
       y = "Predicted MPG")


# Visualize interactions
nba_data_2022 %>%
  mutate(pos_preds = predict(x_pos_nba_lm)) %>%
  ggplot(aes(x = field_goal_percentage, 
             color = position)) +
  geom_point(aes(y = pos_preds),
             alpha = 0.5) +
  theme_bw() +
  facet_wrap(~ position, ncol = 3) +
  labs(x = "FG%", 
       y = "Predicted MPG") +
  geom_smooth(aes(y = min_per_game),
              method = "lm") 

# Fit a model with an interaction term between two variables
pos_int_nba_lm <- lm(min_per_game ~ position + field_goal_percentage +
                       position * field_goal_percentage, 
                     data = nba_data_2022)

nba_data_2022 %>%
  mutate(pos_int_preds = predict(pos_int_nba_lm)) %>%
  ggplot(aes(x = field_goal_percentage, 
             color = position)) +
  geom_point(aes(y = pos_int_preds),
             alpha = 0.5) +
  theme_bw() +
  facet_wrap(~ position, ncol = 3) +
  labs(x = "FG%", 
       y = "Predicted MPG") +
  geom_smooth(aes(y = min_per_game),
              method = "lm")

# Fit your model following transformations
nba_data_2022 <- nba_data_2022 %>%
  mutate(fg_perc_squared = field_goal_percentage^2)
squared_fg_lm <- lm(min_per_game ~ field_goal_percentage + fg_perc_squared, 
                    data = nba_data_2022)
summary(squared_fg_lm)

poly_nine_fg_lm <- lm(min_per_game ~ poly(field_goal_percentage, 9), 
                      data = nba_data_2022)
summary(poly_nine_fg_lm)


# Training and testing models ---------------------------------------------

# Split the sample
n_players <- nrow(nba_data_2022)
train_i <- sample(n_players, n_players / 2, replace = FALSE)
test_i <- (1:n_players)[-train_i]
nba_train <- nba_data_2022[train_i,]
nba_test <- nba_data_2022[test_i,]

# Create two models, both fitted on the training data
candidate_model_1 <- lm(min_per_game ~ poly(field_goal_percentage, 2) + position +
                          position * poly(field_goal_percentage, 2), 
                        data = nba_train)

summary(candidate_model_1)

candidate_model_2 <- lm(min_per_game ~ poly(field_goal_percentage, 2) + position, 
                        data = nba_train)

summary(candidate_model_2)

# Evaluate the models on the test set
model_1_preds <- predict(candidate_model_1, newdata = nba_test)

model_1_mse <- mean((model_1_preds - nba_test$min_per_game)^2)

model_1_mse # Model 1 has the lower MSE, so the model performed better

model_2_preds <- predict(candidate_model_2, newdata = nba_test)

model_2_mse <- mean((model_2_preds - nba_test$min_per_game)^2)

model_2_mse
