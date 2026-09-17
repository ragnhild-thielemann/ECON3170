

library(WDI)
library(tidyverse)
library(countrycode)
library(vdemdata)

#må først behandle demokratiindeksene
vdem_edit <- vdem |>
  select(country_name , v2x_libdem, year) |>
  filter(year == 2020) |>
  mutate(country_name  = countrycode(sourcevar = country_name          , 
                                       origin = "country.name",
                                       destination = "iso2c"))|>
  rename(nationality = country_name  , dem = v2x_libdem)|>
  select(-year)

View(vdem_edit)


hva_er_vanskelig <- performance_by_nationality|> #må konvertere til iso2c, så WDI klarer å beregne 
  mutate(nationality = countrycode(sourcevar = nationality, 
                                 origin = "country.name",
                                 destination = "iso2c"))|>
  left_join(vdem_edit)|>

  mutate(dem_faktor = as.factor(case_when(between(dem,0,0.25) ~ 1,
                         between(dem,0.25,0.5) ~ 2,
                         between(dem,0.5,0.75) ~ 3,
                         between(dem,0.75,1) ~ 4,
                         
    
  )))|>
  mutate(kontinent = countrycode(sourcevar = nationality,
                                 origin = "iso2c",
                                 destination = "continent"
    
  ))
  
View(hva_er_vanskelig)
land <- hva_er_vanskelig$nationality #gjør dette til en vektor av landkoder

total_tibble <- WDI(country = land,
        indicator = "NY.GDP.PCAP.CD", #finner gdp per kapita
        start = 2018, #velger året 2018
        end = 2018)|>
  select(iso2c,NY.GDP.PCAP.CD )|> #veger de relavante kolonnene
  rename(per_capita =NY.GDP.PCAP.CD, nationality = iso2c  )|> #bytter navn, slik at vi kan slå kolonnene sammen
  left_join(hva_er_vanskelig)#slår sammen kolonnene
  
ferdigheter <- total_tibble |>
  filter(!part %in% c("overall")) |> #ikke intressant å se på overordnet score
  mutate(part = as.factor(if_else(part %in% c("listening","speaking"),"listning","writing"))) |> #gir 0 for listning og speeking, og 1 for lesing og skriving
  ggplot(aes(x = per_capita, y = score, color = dem_faktor)) + 
  geom_smooth()
  
ferdigheter

kontinent_mot_demokrati <- total_tibble |>
  ggplot(aes(x = per_capita, y = dem, color = kontinent )) +
  geom_line()

kontinent_mot_demokrati

ggsave("diktaturer_med_laegs.png", plot = ferdigheter)
