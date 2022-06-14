# PURPOSE: K-means clustering of Gapminder data


# Load the data -----------------------------------------------------------

library(tidyverse)
library(dslabs)

gapminder <- as_tibble(gapminder)

gapminder %>%
  ggplot(aes(x=log(gdp))) +
  geom_histogram()


# Clean the data ----------------------------------------------------------

clean_gapminder <- gapminder %>%
  filter(year == 2011, !is.na(gdp)) %>%
  mutate(log_gdp = log(gdp))

clean_gapminder %>%
  ggplot(aes(x=log_gdp, y = life_expectancy)) +
  geom_point(alpha = 0.75) +
  theme_bw()


# Initial unscaled k-means clustering -------------------------------------

init_kmeans <- 
  kmeans(dplyr::select(clean_gapminder,
                       log_gdp, life_expectancy),
         algorithm = "Lloyd", centers = 3,
         nstart = 1)

clean_gapminder %>%
  mutate(country_clusters = 
           as.factor(init_kmeans$cluster)) %>%
  ggplot(aes(x = log_gdp, y = life_expectancy,
             color = country_clusters)) +
  geom_point() + 
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "bottom")


# Scale the variables -----------------------------------------------------

clean_gapminder <- clean_gapminder %>%
  mutate(std_log_gdp = as.numeric(scale(log_gdp, center = TRUE, scale = TRUE)),
         std_life_exp = as.numeric(scale(life_expectancy, center = TRUE, scale = TRUE)))

std_kmeans <- 
  kmeans(dplyr::select(clean_gapminder,
                       log_gdp, std_life_exp),
         algorithm = "Lloyd", centers = 3,
         nstart = 1)

clean_gapminder %>%
  mutate(country_clusters = 
           as.factor(std_kmeans$cluster)) %>%
  ggplot(aes(x = std_log_gdp, y = std_life_exp,
             color = country_clusters)) +
  geom_point() + 
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "bottom")