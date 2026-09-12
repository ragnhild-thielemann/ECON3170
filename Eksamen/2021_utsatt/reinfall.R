

library(tidyverse)
library(docstring)
library(lubridate)
#------------------------------------------------------------
#1. Importerer datasettet
#------------------------------------------------------------

rain <- read_csv("41701_vedlegg_rainfall.csv") |>
  rename(dato = date) #bytter navn på konlonnen over datoer, slik at den ikke blandes med pakken date

#' Vi skal finne hvor mange kommuner som det er gjort målinger i.
#' Dette er antall unike objekter i kolonnen over kommuner. 
#' Vi regner dette ut ved å finne lengden på vektoren over unike kommuner
#' i datasettet
antall_kommuner <- length(unique(rain$knr))

sprintf("Det er %s kommuner i datasettet", antall_kommuner)
# [1] "Det er 430 kommuner i datasettet"

#------------------------------------------------------------
#2. Lager et time-series-plot over daglig regn i Oslo fra 1.
# Juni til 31. August
#------------------------------------------------------------

oslo <- rain |>
  dplyr::filter(knr == 301 ) |> #filtrerer ut oslo
  mutate(dato = as.Date(dato)) |>
  arrange(dato) |>
  select(-knr) #fjerner kommunenummeret


oslo_timeseries <- oslo |>
  dplyr::filter(dato %in% as.Date(ymd("2011-08-01") : ymd("2011-10-01")) ) |>
  ggplot(aes(x = dato, y = rain)) + 
  geom_line() + #lager et linjediagram 
  labs(x = "Dato", y = "Regn", title = "Regn i Oslo")

oslo_timeseries

#------------------------------------------------------------
#3. Skal lage et boksplott over nedbør i Oslo
#------------------------------------------------------------


oslo_boxplot <- oslo |>
  mutate(month = month(dato)) |> #finner måneden til hver observasjon
  select(-dato) |>
  group_by(month) |>
  ggplot(aes(month, rain)) + #har måneder og regn som de ulike aksene
  geom_boxplot(aes(group = month))  #grupperer etter måneder
  


(oslo_boxplot)

#------------------------------------------------------------
#4. Skal finne totalt gjennomsnittlig regn per kommune
#------------------------------------------------------------

total.rainfall <- rain |>
  group_by(knr) |>
  summarise(total = sum(rain))|>
  ungroup() |>
  mutate(null_regn = if_else(total == 0,0,1))

print(sum(total.rainfall$null_regn))
mest_regn <- total.rainfall$knr[total.rainfall$total == max(total.rainfall$total)]
minst_regn <- total.rainfall$knr[total.rainfall$total == min(total.rainfall$total)]

sprintf("%s hadde mest regn, og %s hadde minst regn", mest_regn, minst_regn)


total_fylke <- total.rainfall |>
  mutate(fylke = floor(knr/100))  |>
  summarise(total_fylke = sum(total), mean_fylke = mean(total),.by = fylke)

#------------------------------------------------------------
#5. Skal finne måneden med mest regn for hver kommune
#------------------------------------------------------------


rainiest <- rain |>
  group_by(knr) |>
  slice_max(rain) |>
  group_by(month(dato)) |>
  summarise(n_ = n()) |>
  rename(a = colnames(rainiest)[1]) 

View(rainiest)
