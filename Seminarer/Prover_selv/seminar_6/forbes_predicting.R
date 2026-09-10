
library(tidyverse)
library(tidymodels)
library(workflows)

set.seed(143)
forbes_data <- forbes |>
  select(age,finalWorth,category,country,selfMade,gender)

a = (colnames(forbes_data))

for (h in 1:length(a)){
  miss <- sum(is.na(forbes_data[[h]])) #sumerer over alle tomme kolonner

  print(sprintf("I kolonnen %s mangler det %0f",a[h],miss))
}



forbes_behandlet <-forbes_data |>
  drop_na() |> #fjerner alle linjer der det mangler en variabel
  mutate(country = as.factor(country)) |>
  step_dummy(all_nominal_predictors()) 

delt_data <- initial_split(forbes_data,0.75)
test_data <- training(delt_data)
kontroll <- testing(delt_data)


forbes_recipe <- forbes_behandlet |>
  recipe(finalWorth ~ .)

model <- linear_reg() |>
  set_engine("lm")

forbes_work <- workflow() |>
  add_model(model)|>
  add_recipe(forbes_recipe) |>
  fit(data = test_data)
