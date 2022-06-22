# PURPOSE: Explore model-based clustering


# Load the data -----------------------------------------------------------

library(tidyverse)
nba_pos_stats <- 
  read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/clustering/nba_2022_player_per_pos_stats.csv")
tot_players <- nba_pos_stats %>% filter(tm == "TOT")
nba_player_stats <- nba_pos_stats %>% filter(!(player %in% tot_players$player)) %>% 
  bind_rows(tot_players)
nba_filtered_stats <- nba_player_stats %>% filter(mp >= 125)
head(nba_filtered_stats)


# Load mclust package -----------------------------------------------------

library(mclust)


# Fit GMM -----------------------------------------------------------------

nba_mclust <- Mclust(dplyr::select(nba_filtered_stats, x3pa, trb))

summary(nba_mclust) # Summarize model statistics

plot(nba_mclust) # Selection plots 1-4

nba_mclust$parameters$mean # Mean of clusters

nba_mclust$parameters$variance$sigma # Covariance matrices for Gaussian models

table("Clusters" = nba_mclust$classification,
      "Positions" = nba_filtered_stats$pos)

# Plot the classification clusters
nba_filtered_stats %>%
  mutate(gmm_class = as.factor(nba_mclust$classification),
         gmm_uncertainty = nba_mclust$uncertainty) %>%
  ggplot(aes(x = x3pa,
             y = trb,
             color = gmm_class,
             size = gmm_uncertainty)) +
  geom_point(alpha = 0.5) +
  guides(size = FALSE) + # Hide the "size" legend
  theme_bw() +
  theme(legend.position = "bottom")


# Exploring clustering probabilities --------------------------------------

nba_player_probs <- nba_mclust$z # Matrix of probabilities

colnames(nba_player_probs) <- paste0("Cluster", 1:3) # Assign column names, paste0 concantanates without separation

# Pivot table to group players with their clusters and probabilities
nba_player_probs <- nba_player_probs %>%
  as_tibble() %>%
  mutate(player = nba_filtered_stats$player) %>%
  pivot_longer(contains("Cluster"),
               names_to = "cluster",
               values_to = "prob") 

# View probability distributions
nba_player_probs %>%
  ggplot(aes(x = prob)) +
  geom_histogram() +
  theme_bw() + 
  facet_wrap(~ cluster, nrow = 2) # Create a subplot for each cluster

# Which observations have the greatest uncertainty?
nba_filtered_stats %>%
  mutate(gmm_class = as.factor(nba_mclust$classification),
         gmm_uncertainty = nba_mclust$uncertainty) %>%
  group_by(gmm_class) %>%
  arrange(desc(gmm_uncertainty)) %>%
  slice(1:5) %>%
  ggplot(aes(x = reorder(player, gmm_uncertainty),
             y = gmm_uncertainty)) +
  geom_point() +
  coord_flip() + # Important when working with names - improves readability
  theme_bw() +
  facet_wrap(~ gmm_class, nrow = 3, scales = "free_y") # Make sure all the names are not shown on each plot