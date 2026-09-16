

#installerer pakken for datasettene, og impoterer filene
library(tidyverse)
library(tidytuesdayR)
library(countrycode)

english_speekers <- demo_by_nationality|>
  mutate(totalscore = ((if_else(band == "<4",2,as.numeric(band)))*percent)) |>



  mutate(kontinent = countrycode(sourcevar = nationality, #legger ved kontinentet de er fra
                                 origin = "country.name",
                                 destination = "continent"))|>
  filter(totalscore<2)|> #fjerner ekstremverdiene
  ggplot(aes(x = kontinent, y = totalscore)) + 
  geom_boxplot(aes(group = kontinent))

english_speekers
View(english_speekers)


