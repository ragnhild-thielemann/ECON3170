

library(tidyverse)
library(docstring)
library(tidytuesdayR)
library(tidymodels)
library(workflows)
library(docstring)
library(recipes)

#Henter ut datasettene vi trenger
cafe <- tuesdata$cafe

#Det er bare indeksen som er relevant - tiden det tar å lage en liten cappuchino
cappuchino <- tuesdata$cappuccino_index|>
  select(country, index)

#Da det er lettere å jobbe med et datasett, slår vi dem sammen. 
cafe <- cafe |>
  left_join(cappuchino)


#For en føste øvelse, skal vi finne om tid og pris er korrelert. Dette gjør vi ved å lage et spredningsplott
pris_lonn <- cafe |>
  ggplot(aes(x = hourly_wage_gbp, y = price_gbp)) + 
  geom_smooth() +
  labs(y = "Pris pa en liten cappuchino (i britiske pund)", x = "Timeslønn (i britiske pund", title = "Baristalonn mot pris pa en cappuchino")

ggsave("lonn_mot_pris_cappuchino.png", plot = pris_lonn)


#Ønsker nå å trene en modell, for å predikere hva som styrer prisen på en cappuchino
#Vi setter price_gdp som responsvariabel, og filtrerer de variablene vi vil ha med i analysen. 
#Vi velger de variablene som jobber med britiske pund
set.seed(1234)
maskin_variabler <- cafe |>
  select(price_gbp, urban, suburban, rural, hourly_wage_gbp,index) 



delt_kaffe <- initial_split(maskin_variabler,0.8)
trening_kaffe <- training(delt_kaffe)
test_kaffe <- testing(delt_kaffe)



model_lm <- linear_reg()|>
  set_mode("regression") |>
  set_engine("lm")

model_rf <- 
rec <- recipe(price_gbp ~ . , data = trening_kaffe)|>
  step_dummy(all_nominal()) 

wf_lm<- workflow() |>
  add_recipe(rec) |>
  add_model(model_lm) |>
  fit(trening_kaffe)


predikert <- predict(wf,test_kaffe)


differanse_tabel = tibble(y = test_kaffe$price_gbp, p_lm = predikert$.pred)|>
  mutate(diff_lm = y-p_lm)


ggplot(a,(aes(x = p_lm, y = y))) + geom_smooth()

ggplot(a) +  geom_qq_line(aes(sample = diff)) + geom_qq(aes(sample = diff))