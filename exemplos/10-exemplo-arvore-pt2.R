# Pacotes ------------------------------------------------------------------

library(tidymodels)
library(rpart)
library(rpart.plot)
library(modeldata)  # Que tem a base para o modelo,,,

data("credit_data")
credit_data <- credit_data %>% na.omit()

credit_tree_model <- decision_tree(
   min_n = 31,
   tree_depth = 5,
   cost_complexity = 0.001) %>%
   set_mode("classification") %>%
   set_engine("rpart")

credit_tree_fit <- fit(
  credit_tree_model,
  Status ~.,
  data = credit_data
)

rpart.plot(credit_tree_fit$fit, roundint = FALSE, cex = 0.8)
cp <- as.data.frame(credit_tree_fit$fit$cptable)
cp

credit_data %>% count(Status)

vip::vip(credit_tree_fit$fit)
