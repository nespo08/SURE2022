# PURPOSE: Use PCR and partial least squares models


# Load the data -----------------------------------------------------------

library(tidyverse)
nfl_model_data <- read_rds("Data/nfl_model_data.rds")

# Or as a CSV: nfl_model_data <- read_csv("Data/nfl_model_data.csv")


# Fit PCR -----------------------------------------------------------------

library(pls)
nfl_pcr_fit <- pcr(score_diff ~ ., ncomp = 2, scale = TRUE, data = nfl_model_data)
summary(nfl_pcr_fit)


# To perform PCR, we need to tune the number of principal componen --------

set.seed(2013)
library(caret)

cv_model_pcr <- train(
  score_diff ~ ., 
  data = nfl_model_data, 
  method = "pcr",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("center", "scale"),
  tuneLength = ncol(nfl_model_data) - 1)

ggplot(cv_model_pcr) + theme_bw()