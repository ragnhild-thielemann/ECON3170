
library(tidyverse)

forbes <- read.csv("forbes_2022_billionaires.csv",header = TRUE)
yngst_eldst_rikest <- forbes |>
  filter(age == min(age, na.rm = TRUE)| age == max(age,na.rm = TRUE)| finalWorth == max(finalWorth,na.rm = TRUE)) #finner eldste og yngste i datasettet, der vi fjerner dem som mangler verdier
colnames(forbes)

rikest <- forbes |>
  arrange(desc(finalWorth)) |>
  filter(finalWorth>=finalWorth[100])

sprintf("Gjennomsnittlig formue for de 100 rikeste er %0g millioner USD",(mean(rikest$finalWorth)))

billionerer_i_land <- forbes |>
  group_by(country) |>
  summarise(antall = n()) |>
  arrange(desc(antall))


colnames(forbes)
bar_land <- billionerer_i_land[1:12,]

#sorterer på antall, slik at man får det i stigende rekkefølge
ggplot(bar_land,aes(x = reorder(country, antall), y = antall)) + geom_col()

ggplot(forbes[1:200,]) + geom_line(aes(x = rank, y = finalWorth,color = gender)) + labs(x = "Rangering", y = "Finalworth", title = "Rank mot finalWorth")

#sortere dem på alder og kjønn

old_and_gender <- forbes |>
  mutate(old_man = if_else(age >65 & gender == "M",1,0)) |>
  mutate(young_man = if_else(age <65 & gender == "M",1,0)) |>
  mutate(old_woman = if_else(age >65 & gender == "F",1,0)) |>
  mutate(young_woman = if_else(age <65 & gender == "F",1,0)) |>
  group_by(country)|>
  summarise(old_man = sum(old_man,na.rm = TRUE),
            young_man = sum(young_man,na.rm = TRUE),
            old_woman = sum(old_woman,na.rm = TRUE),
            young_woman = sum(young_woman,na.rm = TRUE))




birth <- forbes|>
  mutate(birthDate = as.Date(birthDate) ) |>
  filter((month(birthDate) != 1 | day(birthDate) != 1 )) |> #bruker logiske oprasjoner til å filtrere dem ut
  mutate(weeks = week(birthDate) ) |>
  group_by(weeks) |>
  summarise(a = n())

ggplot(birth,aes(x = weeks, y = a)) + geom_col()
