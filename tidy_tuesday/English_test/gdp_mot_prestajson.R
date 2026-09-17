

library(WDI)
library(tidyverse)
library(countrycode)
hva_er_vanskelig <- performance_by_nationality|> #må konvertere til iso2c, så WDI klarer å beregne 
  mutate(nationality = countrycode(sourcevar = nationality, 
                                 origin = "country.name",
                                 destination = "iso2c"))|>
  mutate(dem = countrycode(sourcevar = nationality,
                           origin = "iso2c",
                           destination = "vdem"))|>
  mutate(dem = as.factor(case_when(between(dem,0,50) ~ 1,
                         between(dem,50,100) ~ 2,
                         between(dem,100,150) ~ 3,
                         between(dem,150,200) ~ 4,
                         
    
  )))
  

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
  ggplot(aes(x = per_capita, y = score, color = dem)) + 
  geom_smooth()
  
ferdigheter

ggsave("diktaturer_med_laegs.png", plot = ferdigheter)
