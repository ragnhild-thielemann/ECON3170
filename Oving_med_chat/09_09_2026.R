
library(tidyverse)
library(tidymodels)
library(workflows)
library(docstring)
library(glmnet)
library(recipes)

set.seed(142)
find("select") #finner hvilke bibliotek der vi bruker select
#leser inn datafilen
data <- (read.csv("student-mat.csv",header = TRUE,sep = ";"))


data <- data |>
  dplyr::select(sex, age, Mjob, Fjob, traveltime, studytime, failures,absences,G3) |> #viktig ting å være oppemerksom på ved instalasjon av flere pakker. Må derfor spesifisere hvilken pakke man jobber med
  rename(grades = G3) |>
  mutate(across(c(Fjob,Mjob,sex), as.factor)) #man gjør dem til faktorer for hele datasettet, slik at "hjemmeværende" har samme faktor for både test-datasettet og det trente datasette


#deler dataen i en test-gruppe og en kontrollgruppe
delt_data = initial_split(data,0.8)
trening_data = training(delt_data)
kontroll_data = testing(delt_data)


#lager alle faktorene som prediktorer, med grades som outcome-variabel
r <- data |>
  recipe(grades ~ ., data = trening_data) |> #vi trener dataen på trening_data
  step_dummy(all_nominal_predictors()) |> #alle
  step_normalize(all_numeric_predictors())   #normaliserer alle prediktorene


lm_model <- linear_reg() |>
  set_engine("lm")


lasso_model <- linear_reg(
  penalty = 2, 
  mixture = 1) |>
  set_engine("glmnet")

#'lager en workflow, der vi setter inn dataen vi trener inn


w_linear <- workflow() |>
  add_model(lm_model) |> #legger til den linjære regresjonen som modell vi jobber med
  add_recipe(r) |> #legger til recipen, der vi har grades som responsvariabel
  fit(data = trening_data)


utfall <- kontroll_data |>
  mutate(lm = predict(w_linear,kontroll_data)$.pred) |>
  mutate(error_lm = grades-lm)|>
  dplyr::select(grades,lm,error_lm)


w_lasso <- workflow() |>
  add_model(lasso_model) |> #legger til den linjære regresjonen som modell vi jobber med
  add_recipe(r) |> #legger til recipen, der vi har grades som responsvariabel
  fit(data = trening_data)


utfall <- utfall |>
  mutate(lasso = predict(w_lasso,kontroll_data)$.pred) |>
  mutate(error_lasso = grades-lasso)|>
  dplyr::select(grades,lm,error_lm,lasso,error_lasso)


utfall<- utfall |>
  dplyr::select(grades,error_lm,error_lasso)|>
  pivot_longer(cols = c("error_lm","error_lasso"),
               names_prefix = "error_",
               names_to = "Model",
               values_to = "Error")


error <- utfall |>
  group_by(Model)|>
  summarise(error = sum(abs(Error**2)))

View(error)
