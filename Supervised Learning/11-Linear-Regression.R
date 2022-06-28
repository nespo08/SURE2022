# PURPOSE: Initial regression models of life expectancy


# Load the data -----------------------------------------------------------

library(tidyverse)
library(dslabs)
gapminder <- as_tibble(gapminder)
clean_gapminder <- gapminder %>%
  filter(year == 2011, !is.na(gdp)) %>%
  mutate(log_gdp = log(gdp))
clean_gapminder


# Model life expectancy ---------------------------------------------------

clean_gapminder %>%
  ggplot(aes(x = life_expectancy)) +
  geom_histogram(color = "black",
                 fill = "darkblue",
                 alpha = 0.3) +
  theme_bw() +
  labs(x = "Life Expectancy")

# Relationship between life exp. and log(GDP)
gdp_plot <- clean_gapminder %>%
  ggplot(aes(x = log_gdp,
             y = life_expectancy)) +
  geom_point(alpha = 0.5) +
  theme_bw() +
  labs(x = "log(GDP)",
       y = "Life Expectancy") 
gdp_plot

# Fit the model
init_lm <- lm(life_expectancy ~ log(gdp),
              data = clean_gapminder) # Life Exp. as a function of log(GDP)
summary(init_lm)

no_init_lm <- lm(life_expectancy ~ 0 + log(gdp),
              data = clean_gapminder) # Life Exp. as a function of log(GDP) (no intercept)

# Plot with no intercept (we do not shift the data)
gdp_plot <- clean_gapminder %>%
  ggplot(aes(x = log_gdp,
             y = life_expectancy)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm",
              formula = "y ~ 0 + x") +
  theme_bw() +
  labs(x = "log(GDP)",
       y = "Life Expectancy") 
gdp_plot

# Find correlation, which matches the Multiple R^2
with(clean_gapminder, cor(log_gdp, life_expectancy))

with(clean_gapminder, cor(log_gdp, life_expectancy))^2

var(predict(init_lm)) / var(clean_gapminder$life_expectancy) # R^2 ~ prop. of the variance of Y exp. by X


# Making predictions ------------------------------------------------------

# Training data
train_preds <- predict(init_lm)

# Predictions on new data
us_data <- clean_gapminder %>%
  filter(country == "United States")

new_us_data <- us_data %>%
  dplyr::select(country, gdp) %>%
  slice(rep(1,3)) %>% # Make 3 copies of the row
  mutate(adj_factor = c(0.001, 0.21, 0.75),
         log_gdp = log(gdp * adj_factor))

# Create new col with predictions
new_us_data$pred_life_expectancy <-
  predict(init_lm, newdata = new_us_data)
  
# Plot the predictions on top of the log(gdp) plot
gdp_plot + geom_point(data = new_us_data,
                      aes(x = log_gdp,
                          y = pred_life_expectancy),
                      color = "red",
                      size = 6,
                      alpha = 0.8)

# Create a lin. regression line
gdp_plot + geom_smooth(method = "lm")

# If predictions matched obs. data, they would fall on the dotted line
clean_gapminder %>%
  mutate(pred_vals = init_lm$fitted.values) %>%
  ggplot(aes(x = pred_vals,
             y = life_expectancy)) + 
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, 
              intercept = 0, 
              linetype = "dashed",
              color = "red", 
              size = 2) +
  theme_bw()

# Efficient way to add model information to the dataset
clean_gapminder <-
  broom::augment(init_lm, clean_gapminder) # Adds residuals, pred. values, etc.

# Able to plot without creating a new column for fitted values (no mutate)
clean_gapminder %>%
  ggplot(aes(x = .fitted,
             y = life_expectancy)) + 
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, 
              intercept = 0, 
              linetype = "dashed",
              color = "red", 
              size = 2) +
  theme_bw()

# Plot the residuals against pred. values
clean_gapminder %>%
  ggplot(aes(x = .fitted, 
             y = .resid)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "darkred") +
  geom_smooth(se = FALSE) + # Plot the residual mean
  theme_bw()


# Multiple regression -----------------------------------------------------

multi_lm <- lm(life_expectancy ~ log_gdp + fertility,
               data = clean_gapminder)

summary(multi_lm) # Ex: for every one unit increase, log_gdp inc. by 0.8347, but fertility is UNCHANGED

