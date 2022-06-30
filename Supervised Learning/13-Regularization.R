# PURPOSE: Modeling with regularization


# Load the data -----------------------------------------------------------

library(tidyverse)
nfl_teams_data <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/regression_examples/nfl_team_season_summary.csv")
nfl_model_data <- nfl_teams_data %>%
  mutate(score_diff = points_scored - points_allowed) %>%
  # Only use rows with air yards
  filter(season >= 2006) %>%
  dplyr::select(-wins, -losses, -ties, -points_scored, -points_allowed, -season, -team)


# Begin using glmnet ------------------------------------------------------

library(glmnet)

# These do the same thing
model_x <- nfl_model_data %>%
  dplyr::select(-score_diff) %>% # Remove score_diff
  as.matrix()

model_x <- model.matrix(score_diff ~ 0 + ., nfl_model_data) # ~. means as a function of all other variables


model_y <- nfl_model_data$score_diff

# Create the model
init_lm <- lm(score_diff ~., nfl_model_data)

# Organize model summary data and plot coefficient estimates
library(broom)
tidy(init_lm) %>%
  filter(term != "(Intercept)") %>%
  mutate(coef_sign = as.factor(sign(estimate)), # Color-code the sign of the values
         term = fct_reorder(term, estimate)) %>%
  ggplot(aes(x = term, 
             y = estimate, 
             fill = coef_sign)) +
  geom_bar(stat = "identity",
           color = "white") +
  scale_fill_manual(values = c("darkred", "darkblue"),
                    guide = FALSE) +
  coord_flip() +
  theme_bw()

# Use glmnet to visualize coefficients
init_ridge_fit <- glmnet(model_x, model_y,
                         alpha = 0)
plot(init_ridge_fit, xvar = "lambda")

# Perform cross-validation
fit_ridge_cv <- cv.glmnet(model_x, model_y, alpha = 0)
plot(fit_ridge_cv) # Y-axis -> HMSE (smaller = better)

fit_lasso_cv <- cv.glmnet(model_x, model_y, alpha = 1) # Will reduce the # of variables
plot(fit_lasso_cv) # Y-axis -> HMSE (smaller = better)

# Tidy lasso regression
tidy_lasso_coef <- tidy(fit_lasso_cv$glmnet.fit)
tidy_lasso_coef %>%
  ggplot(aes(x = lambda, 
             y = estimate,
             group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = fit_lasso_cv$lambda.min) +
  geom_vline(xintercept = fit_lasso_cv$lambda.1se, 
             linetype = "dashed", 
             color = "red") +
  theme_bw()


# Elastic net - comparing across alphas -----------------------------------

set.seed(2020)
test_fold_id <- sample(rep(1:10, length.out = nrow(model_x)))

# Use cross-validation on diff. alpha values
cv_en_25 <- cv.glmnet(model_x, model_y, 
                      foldid = test_fold_id, alpha = .25)
cv_en_50 <- cv.glmnet(model_x, model_y, 
                      foldid = test_fold_id, alpha = .5)
cv_ridge <- cv.glmnet(model_x, model_y, 
                      foldid = test_fold_id, alpha = 0)
cv_lasso <- cv.glmnet(model_x, model_y, 
                      foldid = test_fold_id, alpha = 1)

# Which had the lowest cross-validation error? 4 - Lasso
which.min(c(min(cv_en_25$cvm), 
            min(cv_en_50$cvm), 
            min(cv_ridge$cvm), 
            min(cv_lasso$cvm)))

# View summary of the model - optimal # of variables is ~ 20
tidy(cv_lasso) %>%
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = cv_lasso$lambda.min) +
  geom_vline(xintercept = cv_lasso$lambda.1se, 
             linetype = "dashed", 
             color = "red") +
  scale_x_log10() + 
  theme_bw()

# This method is improper: we tuned and tested on the same data...

# Instead, tune on the training data, then compare models on the holdout data from that split training set


# Compare models by holdout performance -----------------------------------

set.seed(2020)
nfl_model_data <- nfl_model_data %>% mutate(test_fold = sample(rep(1:5, length.out = n())))
holdout_predictions <- 
  map_dfr(unique(nfl_model_data$test_fold), 
          function(holdout) {
            
            # Separate test and training data:
            test_data <- nfl_model_data %>% filter(test_fold == holdout)
            train_data <- nfl_model_data %>% filter(test_fold != holdout)
            
            # Repeat for matrices
            test_x <- as.matrix(dplyr::select(test_data, -score_diff))
            train_x <- as.matrix(dplyr::select(train_data, -score_diff))
            
            # Train models
            lm_model <- lm(score_diff ~ ., data = train_data)
            ridge_model <- cv.glmnet(train_x, train_data$score_diff, alpha = 0)
            lasso_model <- cv.glmnet(train_x, train_data$score_diff, alpha = 1)
            en_model <- cv.glmnet(train_x, train_data$score_diff, alpha = .5)
            
            # Return tibble of holdout results
            tibble(lm_preds = predict(lm_model, newdata = test_data),
                   ridge_preds = as.numeric(predict(ridge_model, newx = test_x)),
                   lasso_preds = as.numeric(predict(lasso_model, newx = test_x)),
                   en_preds = as.numeric(predict(en_model, newx = test_x)),
                   test_actual = test_data$score_diff, test_fold = holdout) 
          })

# Compute RMSE across folds to evaluate the models
holdout_predictions %>%
  pivot_longer(lm_preds:en_preds, 
               names_to = "type", values_to = "test_preds") %>%
  group_by(type, test_fold) %>%
  summarize(rmse =
              sqrt(mean((test_actual - test_preds)^2))) %>% 
  ggplot(aes(x = type, y = rmse)) + 
  geom_point() + theme_bw() +
  stat_summary(fun = mean, geom = "point", 
               color = "red") + 
  stat_summary(fun.data = mean_se, geom = "errorbar",
               color = "red")

# Although the mean RMSE for "lm" is lower, it is within one RMSE of "en" and "lasso", so we
# can't determine a performance difference; however, "ridge" did perform poorly (high RMSE)