

library(tidyverse)
library(tidymodels)
library(ranger)
library(workflows)

data<- read_tsv("chocolate.tsv") |>
  select(Rating,Cocoa_Percent,Review_Date,Bean_Origin )


splittet_data <- initial_split(data,0.8)
trening_sjok <- training(splittet_data)
kontroll_sjok <- testing(splittet_data)

ggplot(mapping = aes(x = Rating)) + 
  geom_density(aes(fill = "Train"),trening_sjok,alpha = 0.5 ) + 
  geom_density(aes(fill = "Test"),kontroll_sjok, alpha = 0.5 ) 
  
#oppretter en linjær modell
model_lm <- linear_reg()|>
  set_engine("lm")

r <- recipe(Rating ~ ., data = trening_sjok) |>
  step_dummy(all_nominal_predictors())



model_rf <- rand_forest()|>
  set_engine("ranger") |>
  set_mode("regression")
  
  

wf <- workflow() |>
  add_recipe(r)|>
  add_model(model_lm)|>
  fit(trening_sjok)

a = predict(wf,kontroll_sjok)$.pred
print(a)
wf_rf <- workflow() |>
  add_recipe(r)|>
  add_model(model_rf)|>
  fit(trening_sjok)

b = predict(wf_rf,kontroll_sjok)$.pred
print(b)

kontroll_sjok <- kontroll_sjok |>
  bind_cols(lin = a) |>
  bind_cols(rand = b) |>
  mutate(error_lin = (Rating-lin)**2, error_rand = (Rating-rand)**2)|>
  
  summarise(MSE_lin = mean(error_lin), MSE_ra = mean(error_rand))
View(kontroll_sjok)
