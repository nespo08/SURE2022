# PURPOSE: Demonstrate logistic regression models for FGs


# Load the data -----------------------------------------------------------

library(tidyverse)
nfl_fg_attempts <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/glm_examples/nfl_fg_attempt_data.csv")
nfl_fg_attempts


# Fit logit model ---------------------------------------------------------

init_logit <- glm(is_fg_made ~ kick_distance, # is_fg_made is a function of kick_dist
                  data = nfl_fg_attempts,
                  family = "binomial") 

nfl_fg_attempts %>%
  mutate(pred_prob = init_logit$fitted.values) %>% # Mutate to add col. with predicted probs
  ggplot(aes(x = kick_distance)) +
  geom_line(aes(y = pred_prob), color = "blue") +
  geom_point(aes(y = is_fg_made),
             alpha = 0.25, color = "orange") +
  theme_bw()

summary(init_logit)


# Make logistic predictions -----------------------------------------------

pred_fg_outcome <- ifelse(init_logit$fitted.values > 0.5,
                           "make", "miss")

# Use a confusion matrix to assess model classification performance
table("Predictions" = pred_fg_outcome, 
      "Observed" = nfl_fg_attempts$is_fg_made)

# In-sample misclassification rate
mean(ifelse(fitted(init_logit) < 0.5, 0, 1) != nfl_fg_attempts$is_fg_made)

# Brier score: actual classification - probability
mean((nfl_fg_attempts$is_fg_made - fitted(init_logit))^2)


# Calibration plots -------------------------------------------------------

nfl_fg_attempts %>%
  mutate(pred_prob = init_logit$fitted.values,
         bin_pred_prob = round(pred_prob / 0.05) * 0.05) %>% # Create bins for probabilities (size: 0.05)
  group_by(bin_pred_prob) %>%
  summarize(n_attempts = n(),
            bin_actual_prob = mean(is_fg_made)) %>%
  ggplot(aes(x = bin_pred_prob,
             y = bin_actual_prob)) +
  geom_point(aes(size = n_attempts)) +
  geom_smooth(se = FALSE) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = "dashed") + 
  coord_equal() +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1)) +
  labs(x = "Estimated make probability",
       y = "Observed make probability",
       size = "Number of attempts") +
  theme_bw() +
  theme(legend.position = "bottom")


# Leave one season out - cross validation ---------------------------------

nfl_fg_loso_cv_preds <- # generate holdout predictions for every row based season
  map_dfr(unique(nfl_fg_attempts$pbp_season), 
          function(season) {
            # Separate test and training data:
            test_data <- nfl_fg_attempts %>%
              filter(pbp_season == season)
            train_data <- nfl_fg_attempts %>%
              filter(pbp_season != season)
            # Train model:
            fg_model <- glm(is_fg_made ~ kick_distance, data = train_data,
                            family = "binomial")
            # Return tibble of holdout results:
            tibble(test_pred_probs = predict(fg_model, newdata = test_data,
                                             type = "response"),
                   test_actual = test_data$is_fg_made,
                   test_season = season) 
          })

# Misclassification rate
nfl_fg_loso_cv_preds %>%
  mutate(test_pred = ifelse(test_pred_probs < .5, 0, 1)) %>%
  summarize(mcr = mean(test_pred != test_actual))

# Brier score
nfl_fg_loso_cv_preds %>%
  summarize(brier_score = mean((test_actual - test_pred_probs)^2))

# Holdout performance by season
nfl_fg_loso_cv_preds %>%
  mutate(test_pred = ifelse(test_pred_probs < .5, 0, 1)) %>%
  group_by(test_season) %>%
  summarize(mcr = mean(test_pred != test_actual)) %>%
  ggplot(aes(x = test_season, y = mcr)) +
  geom_bar(stat = "identity", width = .1) + geom_point(size = 5) +
  theme_bw() +
  scale_x_continuous(breaks = unique(nfl_fg_loso_cv_preds$test_season))
