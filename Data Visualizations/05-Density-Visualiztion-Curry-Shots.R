# PURPOSE: Density estimation visualizations


# Load data ---------------------------------------------------------------

library(tidyverse)
curry_shots <- 
  read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/xy_examples/curry_2022_shots.csv")
head(curry_shots)


# Make histograms ---------------------------------------------------------

curry_shots %>% 
  ggplot(aes(x = shot_distance)) +
  geom_histogram(breaks = seq(0,50,by=5)) +
  theme_bw()

# Noisy / spikey plot (smallest binwidth given the data we have)

curry_shots %>% 
  ggplot(aes(x = shot_distance)) +
  geom_histogram(binwidth = 1) +
  theme_bw()

# Oversmooth/flat plot (largest binwidth given the data we have)

curry_shots %>% 
  ggplot(aes(x = shot_distance)) +
  geom_histogram(binwidth = 25) +
  theme_bw()

# Define more appropriate hist. breaks
curry_shots %>% 
  ggplot(aes(x=shot_distance)) +
  geom_histogram(binwidth = 1, center = 0.5, closed = "left") + # Shift bins so zero is not far left
  theme_bw()


# Density curves ----------------------------------------------------------

curry_shots %>%
  ggplot(aes(x=shot_distance)) +
  geom_density(adjust = 0.5) + # Shrink h
  geom_rug(alpha = 0.5) +
  theme_bw()

# ECDF
library(patchwork)
curry_density_curve <- curry_shots %>%
  ggplot(aes(x=shot_distance,
             color = is_shot_made)) +
  geom_density() +
  geom_rug(alpha = 0.5) +
  theme_bw()
curry_ecdf <- curry_shots %>%
  ggplot(aes(x=shot_distance,
             color = is_shot_made)) +
  stat_ecdf() +
  theme_bw()
curry_density_curve + curry_ecdf + plot_layout(guides = "collect") # Patchwork allows side-by-side plots


# Stacked density curves - ridge plots (not super useful) ------------------------------------

library(ggridges)
curry_shots %>%
  ggplot(aes(x = shot_distance,
             y = shot_type)) +
  geom_density_ridges(rel_min_height = 0.01) +
  theme_bw()


# 2D shot location density estimation -------------------------------------

# Modify column values
curry_shots <- curry_shots %>%
  mutate(shot_x = -shot_x/10,
         shot_y = shot_y / 10)

# Using density 2d and contour mapping
curry_shots %>%
  ggplot(aes(x=shot_x, 
             y = shot_y)) +
  geom_point(alpha = 0.3) +
  geom_density2d(adjust = 0.1) + # Use contour mapping to show relative probabilities
  theme_bw() +
  coord_equal() # One unit on y-axis = one unit on x-axis

# Filled areas displaying modes (heatmap)
curry_shots %>%
  ggplot(aes(x=shot_x, 
             y = shot_y)) +
  stat_density2d(geom = "raster",
                 contour = FALSE,
                 adjust = 0.75,
                 aes(fill = after_stat(density))) +
  scale_fill_gradient(low = "white", high = "darkred") +
  theme_bw() +
  coord_equal() # One unit on y-axis = one unit on x-axis

# Hexagonal heatmap display
library(hexbin)
curry_shots %>%
  ggplot(aes(x = shot_x, 
             y = shot_y)) + 
  geom_hex(binwidth = c(1, 1)) +
  scale_fill_gradient(low = "white", high = "darkred") +
  theme_bw() +
  coord_fixed()

# Hexagonal heatmap display - shooting efficiency
curry_shots %>%
  ggplot(aes(x = shot_x,
             y = shot_y,
             z = is_shot_made, group = -1)) +
  stat_summary_hex(binwidth = c(1,1), 
                   fun = mean) + # Need statistical summary of z-dimension (we use the mean)
  scale_fill_gradient(low = "white", high = "darkred") + 
  theme_bw() +
  theme(legend.position = "bottom") +
  coord_fixed()