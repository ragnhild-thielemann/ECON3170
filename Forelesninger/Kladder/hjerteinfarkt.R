
library(tidymodels)
library(nnet)
library(tidyverse)
library(skimr) #brukes for å fjerne tomme observasjoner
set.seed(143)
raw<- read_csv("Heart_Attack.csv") |>
  mutate(target = factor(target))

#splitter dataen i en test-del og en kontroll-del
split_raw = initial_split(raw,0.8)
test_heart = testing(split_raw)
trening_heart = training(split_raw)



lm_model <- multinom_reg() |>
  set_engine("nnet") #linjær regresjon, med det enkleste mininmeringsproblemet

recipe_heart <- recipe(target ~ ., data = trening_heart) #bruker alle datasettene som kovariater
  
work <- workflow()|>
  add_recipe(recipe_heart)|>
  add_model(lm_model) |>
  fit(trening_heart)

p <- work |>
  predict(test_heart ) |>
  bind_cols(test_heart) |>
  select(.pred_class, target) 


a = as.numeric(p$.pred_class)
b =as.numeric(p$target)

c = cor(a,b)
c

a <- p |>
            filter(target == "2")
print(nrow(a))
print(to)