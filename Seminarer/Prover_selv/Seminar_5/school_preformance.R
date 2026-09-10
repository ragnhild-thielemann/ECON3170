
library(tidyverse)
library(rsample)
library(workflows)
library(tidymodels) #importerer bibliotek for maskinlæring
library(docstring)
#leser inn datafilen
data <- tibble(read.csv("student-mat.csv", sep = ";", header = TRUE)) #understrek for å lage en tibble

set.seed(100) #gjør at tilfeldige genereringer blir likt hver gang
data <- data |>
  rename(grades = G3)|> #endrer navnet på sluttkarakteren
  select(sex,age,Mjob, Fjob, traveltime, studytime,failures,absences,grades)|> #velger ut de relevante forkalringsvariablene
  mutate(across(c(sex,Mjob,Fjob), as.factor)) #gjør kjønn og foreledrenes jobber som forklaringsvariabler

#vi deler datasettet i en test-del og en treningsdel før vi begynner behandlingen
data_split <- initial_split(data,0.8)
train_data <- training(data_split)
test_data <- testing(data_split)

#' Vi må konvertere tekststrengene våre til numeriske variabler, for at maskinene skal lære av dem

recipe <- recipe(grades ~ sex + age + Mjob + Fjob + traveltime + studytime + failures + absences, data = train_data) |> #anser karaktere til å være en betinget variat av disse
  step_dummy(all_nominal_predictors()) #stanariserer alle variablene

#' I recipen gjør vi karakterene til responsvariablen, og alle de andre som prediktorer. Dette bruker vi vidre når vi tilpasser ulike modeller til datasettet
#' 
#' Vi begynner med vanlig, linjær regresjon

lm_model <- linear_reg() |>
  set_engine("lm") #lager oppsettet for linjær regresjon

#vi trener modellen på train-dataen
lm_fit <- workflow() |>
  add_recipe(recipe) |>
  add_model(lm_model)|>
  fit(data = train_data)

#vi kjører modellen på test-dataen, da den er ubehandlet
lm_results <- test_data |> 
  
  mutate(p =  predict(lm_fit, test_data)$.pred,
         error = p - grades) |>
  select(grades, p, error)

ggplot(lm_results) + geom_point(aes(x = grades, y = error))