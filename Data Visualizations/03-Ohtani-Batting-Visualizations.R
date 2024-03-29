# Load packages -----------------------------------------------------------

library(tidyverse)
ohtani_batted_balls <- 
  read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/xy_examples/ohtani_2021_batted_balls.csv")
head(ohtani_batted_balls)


# Make a bar chart (1D categorical data) of batted ball type ------------------------------------

ohtani_batted_balls %>%
  ggplot(aes(x = batted_ball_type)) +
  geom_bar() +
  theme_bw()

# Display proportions on the y-axis
ohtani_batted_balls %>%
  ggplot(aes(x = batted_ball_type)) +
  geom_bar(aes(y = after_stat(count) / sum(after_stat(count)))) +
  theme_bw()

# Or...
ohtani_batted_balls %>% 
  group_by(batted_ball_type) %>%
  summarize(count = n(), 
            .groups = "drop") %>%
  #ungroup() %>%
  mutate(total = sum(count), 
         prop = count / total,
         se = sqrt(prop * (1-prop)/total), # Add std. error calculations
         lower = prop - 2 * se,
         upper = prop + 2* se,
         batted_ball_type = fct_reorder(batted_ball_type, prop)) %>% # Reorder the batted ball types based on prop. variable
  ggplot(aes(x = batted_ball_type)) +
  geom_bar(aes(y = prop), 
           stat = "identity") + # We created the variable 'prop'
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                color = "red") +
  theme_bw()

# Visualize exit velocity (1D cont. data) ------------------------

# Create a basic boxplot (which are terrible, don't use them)
ohtani_batted_balls %>% 
  ggplot(aes(y = exit_velocity)) +
  geom_boxplot(aes(x = "")) +
  theme_bw() +
  coord_flip() # Flips boxplot

# Create a histogram (better-ish)
ohtani_batted_balls %>% 
  ggplot(aes(x = exit_velocity)) +
  geom_histogram(bins = 20) +
  theme_bw()

# Create a beeswarm plot (good for smaller datasets)
library(ggbeeswarm)
ohtani_batted_balls %>%
  ggplot(aes(y = exit_velocity)) +
  geom_beeswarm(aes(x = ""),
                cex = 3) +
  theme_bw() +
  coord_flip()

# Create a violin plot with a boxplot
ohtani_batted_balls %>%
  ggplot(aes(y = exit_velocity, 
             x = "")) +
  geom_violin() +
  geom_boxplot(width = 0.2) +
  theme_bw() +
  coord_flip()

# Display the empirical CDF (ECDF) with a rug plot (which show individual observations)
ohtani_batted_balls %>%
  ggplot(aes(x = exit_velocity)) +
  stat_ecdf() +
  geom_rug(alpha = 0.25) +
  theme_bw()

# Scatterplots (2D cont. data) --------------------------------------------

ohtani_batted_balls %>%
  ggplot(aes(x = exit_velocity, y = launch_angle)) +
  geom_point(alpha = 0.45) +
  geom_rug(alpha = 0.35) +
  theme_bw()
  

