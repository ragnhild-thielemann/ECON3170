
library(tidyverse)
library(readxl)
library(countrycode)
library(docstring)

?countrycode
co2 <- read_xlsx("co2.xlsx")

#lager tibbelen der vi tar inn kumulative plott
cummulative_plot <- co2 |>
  filter(Code %in% c("CHN","USA")) |>
  ggplot(aes(x = Year, y = co2 ,color = Code )) + 
  geom_line()

#lager tibbelen der vi tar å lager økningen i plottene
increase_plot <- co2 |>
  group_by(Code)|>
  mutate(increase = co2-lag(co2)) |>
  filter(Code %in% c("CHN","USA")) |>
  filter(Year %in% 1900:2020) |>
  ggplot(aes(x = Year, y = increase ,color = Code )) + 
  geom_line()

increase_plot

#summerer totale utslipp per år. Må ikke gruppere hele tabbellen, da det føkkes opp
global <- co2 |>

  summarise(total = sum(co2),.by = Year) 

#lagrer plottet som en tibble
global_plot <- global |>
  ggplot(aes(x = Year, y = total)) + 
  geom_line() 

#lager en tibble, der vi filtrerer ut usa
us <- co2 |>
  filter(Code %in% c("USA")) |>
  rename(co2_usa = co2) |>
  select(Year,co2_usa)

#lager en tibbel der vi filtrer ut kina
ch <- co2 |>
  filter(Code %in% c("CHN")) |>
  rename(co2_china = co2) |>
  select(Year,co2_china)

#merger disse sammen med de globale utslippene per år
global <- global |>
  left_join(us) |>
  left_join(ch) |>
  mutate(share_us = co2_usa/total, share_ch = co2_china/total) |>
  select(Year,share_us,share_ch)|>
  pivot_longer(cols = -(Year),
                names_prefix = "share_",
               names_to = "Land",
               values_to = "Andel")

global_plot <- global |>
  ggplot(aes(x = Year, y = Andel,color = Land)) + 
  geom_line()

#' Bruker biblioteket countrycode til å finne kontinentet landenene ligger i. 
#' Bruker så mutate til å legge dette til som en ekstra kolonne
co2_kontinent <- co2 |>
  mutate(continent = countrycode(Code, origin  ="iso3c",   destination = "continent"))|>
  select(continent,Year,co2)

co2_kontinent_kummulativ <- co2_kontinent|>
  group_by(Year,continent)|>
  summarise(cummulative = sum(co2)) |>
  ggplot(aes(x = Year, y = cummulative, color =continent) ) + 
  geom_line() + 
  labs(x = "Year", y = "Kummulative utslipp", title = "Verdens kummulative utslipp", color = "Kontinent")


co2_kontinent_kummulativ


co2_kontinent_okning <- co2_kontinent |>
  group_by(continent)|>
  mutate(ok = co2- lag(co2)) |>
  group_by(Year,continent)|>
  summarise(total = sum(ok)) |>
  ggplot(aes(x = Year, y = total, color =continent) ) + 
  geom_line() + 
  labs(x = "Year", y = "Okning i  utslipp", title = "Okning i utslipp", color = "Kontinent")



(co2_kontinent_okning)