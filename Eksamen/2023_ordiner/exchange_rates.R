
library(docstring)
library(readxl) #for å lese excel-filer
#a Lager et tidsserieplott
library(tidyverse)
euro <- read.csv("euro.csv") |>
  mutate(t = as.Date(date))
euro |> ggplot() + geom_point(aes(x = t, y = rate))

#b lager en liste for gjennomsnittlig valuttakurs
#lager en ny tibble, der jeg splitter opp år og måned, og finner gjennomsnittlig valuttakurs per måned
avereage <- euro |> 
  group_by(Year = year(t), Month = month(t)) |> 
  summarise(Mean= mean(rate,na.rm = TRUE))

#c

top <- euro |>
  mutate(y = year(t)) |>
  group_by(y) |>
  slice_max(rate,n = 1) |>
  rename(top_date = t, max_rate = rate)

bottom <- euro |>
  mutate(y = year(t)) |>
  group_by(y) |>
  slice_min(rate,n = 1) |>
  rename(min_date = t, min_rate = rate)

bottom <- bottom |>
  full_join(top) |>
  relocate(y) #setter y først

#' Vi får da et tidy format, med den tabbellen vi ønsker


#d

nb_rates <- read_xlsx("nb_rates.xlsx") #importerer norges banks styringsrente fra 1986 til 2023 

nb_rates |> ggplot() +
  geom_step(aes(x = date, y = policy_rate)) + #lager det som en stepfunksjon, få å få med den diskrete endringen av styringsrenten
  labs(x = "Tid", y = "Styringsrente", title = "Styringsrente over tid")


#e


nb_rates <- nb_rates |>
  mutate(date = as.Date(date))|>
  rename(t = date)

View(euro)
View(nb_rates)

euro_it <- euro |>
  left_join(nb_rates) |>
  fill(policy_rate)|> #fyller ut dataen på alle punktene, til tross for at vi ikke har data for alle datoene
  filter(year(t) %in% 2014:2022)

ggplot(euro_it) + geom_line(aes(x = t, y = rate)) + geom_line(aes(x = t, y = policy_rate))

#lager det som en linjær modell

model = lm(euro_it$rate ~ euro_it$policy_rate)

summary(model)

