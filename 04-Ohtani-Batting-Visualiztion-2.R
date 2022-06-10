# PURPOSE: Create 2D categorical visualization with some inference

# Load data ---------------------------------------------------------------

library(tidyverse)
ohtani_batted_balls <- 
  read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/xy_examples/ohtani_2021_batted_balls.csv")
head(ohtani_batted_balls)

# Explore pitch type ------------------------------------------------------

table(ohtani_batted_balls$pitch_type)

ohtani_batted_balls <- ohtani_batted_balls %>%
  filter(!is.na(pitch_type)) %>% # Remove rows missing pitch type
  mutate(pitch_type = fct_recode(pitch_type, "Changeup" = "CH",
                                 "Breaking Ball" = "CU",
                                 "Fastball" = "FC",
                                 "Fastball" = "FF",
                                 "Fastball" = "FS",
                                 "Breaking Ball" = "KC",
                                 "Fastball" = "SI",
                                 "Breaking Ball" = "SL")) # Update pitch_type variables

table(ohtani_batted_balls$pitch_type) # Now, we only have three pitch types

# Chi-squared test (1D data)
chisq.test(table(ohtani_batted_balls$pitch_type))

# 2D visuals with pitch type and batted ball type -------------------------

# Stacked bar plot (marginal dist.)
ohtani_batted_balls %>%
  ggplot(aes(x=batted_ball_type, fill = pitch_type)) +
  geom_bar() +
  ggthemes::scale_fill_colorblind() +
  theme_bw()

# Side-by-side bar plot (conditional dist.)
ohtani_batted_balls %>%
  ggplot(aes(x=batted_ball_type, fill = pitch_type)) +
  geom_bar(position = "dodge") + # "dodge" -> side-by-side
  ggthemes::scale_fill_colorblind() +
  theme_bw()

# Contingency tables ------------------------------------------------------

table("Pitch Type" = ohtani_batted_balls$pitch_type,
      "Batted Ball Type" = ohtani_batted_balls$batted_ball_type)

proportions("Pitch Type" = ohtani_batted_balls$pitch_type,
      "Batted Ball Type" = ohtani_batted_balls$batted_ball_type)

table("Pitch Type" = ohtani_batted_balls$pitch_type,
      "Batted Ball Type" = ohtani_batted_balls$batted_ball_type) %>%
  proportions()


# Chi-squared test (2D data)
table("Pitch Type" = ohtani_batted_balls$pitch_type,
      "Batted Ball Type" = ohtani_batted_balls$batted_ball_type) %>%
  chisq.test()

# Visualize independence with a mosaic plot -------------------------------

table("Pitch Type" = ohtani_batted_balls$pitch_type,
      "Batted Ball Type" = ohtani_batted_balls$batted_ball_type) %>%
  mosaicplot(main = "Relationship between batted ball type and pitch type?",
             shade = TRUE) # Shade by Pearson residuals (how far we differ from exp. independence)
