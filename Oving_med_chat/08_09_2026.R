

library(tidymodels)
library(tidyverse)
library(MASS)
library(workflows)
set.seed(123)
boston <- as_tibble(MASS::Boston)

t_boston <- initial_split(boston,0.8)
test_boston <- testing(t_boston)
training_boston <- training(t_boston)


boston_re <- training_boston |>
  recipe(medv ~ lstat + black + ptratio + tax + rad + dis + age + rm + nox + chas + indus + zn + crim) |>
  step_normalize(all_predictors()) 
  
lm_model <- linear_reg() |>
  set_engine("lm")

lin_wf <- workflow() %>% 
  add_recipe(boston_re) %>% 
  add_model(lm_model)
  

lin_fit <- fit(lin_wf, data = training_boston)
tidy(lin_fit)