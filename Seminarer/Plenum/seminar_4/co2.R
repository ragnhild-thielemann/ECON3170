

library(tidyverse)
library(countrycode)
library(readxl)
library(sf)
library(docstring) 
library(rnaturalearth)
#' Når jeg jobber med datasettet, passer jeg på at jeg aldri endrer stamsettet mitt co2. Alle andre tibbler jeg lager under arbeidet, lagrer jeg med nye navn

co2<- read_xlsx("co2.xlsx")

colnames(co2)
usa_kina <- co2 |>
  filter(Code %in% c("USA","CHN")) |>
  ggplot(aes(x = Year, y = co2, color = Entity)) + 
  geom_line() + 
  labs(title = "Kummulative utslipp")

usa_kina
usa_kina_increase <- co2 |>
  filter(Code %in% c("USA", "CHN")) |>
  group_by(Code) |>
  mutate(increase = ((co2-lag(co2))))|>
  filter(between(Year,1950,2020)) |> 
  
  mutate(increase = if_else(increase == "Inf",NaN,increase)) |> #gjør at uendelig også blir en uaktuell verdi
  drop_na()|> #fjerner alle steder der vi ikke har data, slik at vi bare sitter igjen med reele datasett
  ggplot(aes(x = Year, y = increase, color = Entity)) +
  geom_line() + 
  labs(x = "Year", y = "Okning i utslipp" , title = "Okning i Utslipp")

usa_kina_increase

global <- co2 |>
  summarise(total_utslipp = sum(co2),.by = Year)


ggplot(data = global, aes(x = Year, y = total_utslipp)) + 
  geom_line() +
  labs(x = "Year", y = "Totale utslipp", title = "Verdens totale utslippp")


us <- co2 |>
  filter(Code %in% c("USA","CHN"))|>
  group_by(Year)|>
  filter(Year>1800) |>
  select(-Entity) |>
  pivot_wider(names_prefix = "Co2",
              names_from = Code, 
              values_from = co2)|>
  left_join(global) |>
  mutate(andel_usa = Co2USA/total_utslipp, andel_kina = Co2CHN/total_utslipp)|>
  select(Year, andel_usa, andel_kina)|>
  pivot_longer(cols = -Year,
               names_prefix =  "andel_",
               names_to = "Land",
               values_to = "Utslipp")



ggplot(us, aes(x = Year, y = Utslipp, color = Land )) + 
  geom_line() + 
  labs(title = "Uslipp som andel av verdens totale")



co2_kontinent <- co2 |>
  mutate(kontinent = countrycode(Code,
                                 "iso3c",
                                 "continent"))|>
  group_by(Year,kontinent )|>
  summarise(total_kontinent = sum(co2))



ggplot(co2_kontinent, aes(x = Year, y = total_kontinent, fill = kontinent)) +
  geom_area() + 
  labs(title = "Totale utslipp fordelt pa land")
  
#------------------------------------------------------------------------------------
#Ønsker å finne ut hvilke land som har økt utslippene sine mest fra 2000 til 2015
#------------------------------------------------------------------------------------

library(tidyverse)
emmisions_by_country <- co2 |>
  filter(Year %in% c(2000,2015)) |> #plukker ut verdiene for 2000 og 2015
  select(Code, Year, co2) |>
  pivot_wider(names_prefix = "co2_",
              names_from = Year,
              values_from = co2)|>
  mutate(ok = (co2_2015-co2_2000)/15)|> #finner økningen per år
  select(Code, ok)

View(emmisions_by_country)

#laster inn verdens land, slik at vi kan behandle dem. Vi får da geometisen til et land
world <- ne_countries(returnclass = "sf") |>
  select(iso_a3) |> #velger bare iso3c kolonnen, og beholder de geometiske kordinatne til landet fra datasettet
  rename(Code = iso_a3) |> #endrer kolonnenavnet, slik at vi kan merge datasettene sammen
  left_join(emmisions_by_country) #slår sammen datasettene

View(world)


ggplot(data = world) + 
  geom_sf(aes(fill = ok)) + #fyller landene med kummulerte co2-utslipp 
  labs(title = "Okning i utslipp fra 2000 til 2015")