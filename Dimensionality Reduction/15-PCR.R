# PURPOSE: Use PCR and partial least squares models


# Load the data -----------------------------------------------------------

library(tidyverse)
nfl_model_data <- read_rds("Data/nfl_model_data.rds")

# Or as a CSV: nfl_model_data <- read_csv("Data/nfl_model_data.csv")


# Fit PCR -----------------------------------------------------------------

library(pls)
nfl_pcr_fit <- pcr(score_diff ~ ., ncomp = 2, scale = TRUE, data = nfl_model_data)
summary(nfl_pcr_fit)


# To perform PCR, we need to tune the number of principal components --------

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


#  Perform PLS as supervised dim. reduction -------------------------------

set.seed(2013)
cv_model_pls <- train(
  score_diff ~ ., 
  data = nfl_model_data, 
  method = "pls",
  trControl = 
    trainControl(method = "cv", number = 10,
                 selectionFunction = "oneSE"), 
  preProcess = c("center", "scale"),
  tuneLength = ncol(nfl_model_data) - 1)

ggplot(cv_model_pls) + theme_bw() 


# Variable importance measures --------------------------------------------

library(vip)
vip(cv_model_pls, num_features = 10,
    method = "model") + # All these variables are strongly correlated w/ each other
  theme_bw()

# PDPs display the change in the average predicted response as the predictor varies over their marginal distribution
library(pdp)
partial(cv_model_pls, "offense_total_epa_pass", plot = TRUE) # How does resp. change as we add variables