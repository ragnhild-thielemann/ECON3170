
library(docstring)
library(readxl) #for å lese excel-filer
#a Lager et tidsserieplott
library(tidyverse)
euro <- read.csv("euro.csv") 
euro |> ggplot() + geom_point(aes(x = t, y = rate))

#b lager en liste for gjennomsnittlig valuttakurs
#lager en ny tibble, der jeg splitter opp år og måned, og finner gjennomsnittlig valuttakurs per måned
avereage <- euro |> 
  group_by(Year = year(t), Month = month(t)) |> 
  summarise(Mean= mean(rate,na.rm = TRUE))

#c

task_c <- euro |> filter(year(t)>=2014)|>#begynner 1. januar 2014
  group_by(year(t)) |> #gruperer på år
  summarise(maks = max(rate), minst = min(rate)) #finner største og minste verdi, en totuppel i summarisen

#' Vi får da et tidy format, med den tabbellen vi ønsker


#d

nb_rates <- read_xlsx("nb_rates.xlsx") #importerer norges banks styringsrente fra 1986 til 2023 

nb_rates |> ggplot() + geom_line(aes(x = date, y = policy_rate)) + labs(x = "Tid", y = "Styringsrente", title = "Styringsrente over tid")


#e
colnames(euro) <- c("date","kurs")

nb_rates <- nb_rates |>
  filter(date > min(euro$date)) |> #begrenser til der vi har data for valutakursen, da dette er området det gir mening å jobbe med
  arrange((date))  #gjør dem i stigende rekkefølge, slik at vi får samme format som for euro-datesettet


print(nrow(euro)/12)
print(nrow(nb_rates))


View(euro)
View(nb_rates)
