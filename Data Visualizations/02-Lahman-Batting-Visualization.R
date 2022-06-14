# Load packages -----------------------------------------------------------

library(tidyverse)
library(Lahman)

# Create year summary -----------------------------------------------------

Batting <- as_tibble(Batting)

year_batting_summary <- Batting %>%
  filter(lgID %in% c("AL", "NL")) %>%
  group_by(yearID) %>%
  summarize_at(vars(HR, SO, BB, AB, H),
               sum, na.rm = TRUE) %>%
  mutate(batting_avg = H / AB)


# Construct basic ggplot examples -----------------------------------------

year_batting_summary %>% 
  ggplot(aes(x = yearID, y = SO)) +
  geom_point(alpha = 0.75, size = 0.85) +
  geom_line() +
  scale_x_continuous(limits = c(2000,2021)) +
  scale_y_continuous(breaks = seq(0, 45000, by = 15000))

# Map more variables
year_batting_summary %>% 
  ggplot(aes(x = yearID, y = SO)) +
  geom_point(alpha = 0.75, aes(color = HR, size = BB)) +
  geom_line(color = 'darkred', linetype = 'dashed') +
  scale_color_gradient(low = 'darkblue', high = 'darkorange') +
  scale_size_continuous(breaks = seq(0, 15000, by = 2500)) +
  labs(x = 'Year', y = 'Strikeouts', color = 'Homeruns', size = 'Walks', 
       title = "The rise of MLB's three true outcomes", caption = 'Data courtesy of Lahman') +
  theme_bw() +
  theme(legend.position = 'bottom', plot.title = element_text(size = 15)) +
  stat_smooth() # Smoothing regression

# Simplify everything!
year_batting_summary %>%
  select(yearID, HR, SO, BB) %>%
  rename(HRs = HR, Strikeouts = SO,
         Walks = BB) %>%
  pivot_longer(HRs:Walks,
               names_to = "stat", 
               values_to = "value") %>%
  ggplot(aes(x = yearID, y = value)) +
  geom_line(color = "darkblue") +
  geom_point(color = "darkblue") +
  facet_wrap(~ stat, #<<
             scales = "free_y", ncol = 1) + #<<
  labs(x = "Year", y = "Total of statistic",
       title = "The rise of MLB's three true outcomes",
       caption = "Data courtesy of Lahman") +
  theme_bw() +
  theme(strip.background = element_blank())
