

library(WDI)
library(tidyverse)
library(countrycode)
hva_er_vanskelig <- performance_by_nationality|> #må konvertere til iso2c, så WDI klarer å beregne 
  mutate(nationality = countrycode(sourcevar = nationality, 
                                 origin = "country.name",
                                 destination = "iso2c"))
  

land <- hva_er_vanskelig$nationality #gjør dette til en vektor av landkoder

A <- WDI(country = land,
        indicator = "NY.GDP.PCAP.CD", #finner gdp per kapita
        start = 2018, #velger året 2018
        end = 2018)|>
  select(iso2c,NY.GDP.PCAP.CD )|> #veger de relavante kolonnene
  rename(per_capita =NY.GDP.PCAP.CD, nationality = iso2c  )|> #bytter navn, slik at vi kan slå kolonnene sammen
  left_join(hva_er_vanskelig) #slår sammen kolonnene

View(A)
