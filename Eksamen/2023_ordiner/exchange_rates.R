
library(docstring)
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