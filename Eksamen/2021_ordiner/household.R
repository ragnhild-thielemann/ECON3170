
library(tidyverse)
library(readxl)
data <- read_xlsx("cps.xlsx")

mean <- data |>
  group_by(female) |>
  summarise(M = mean(ahe))


data <- data |> 
  mutate (yearincome = ahe*(8*5*50)) |>
  arrange(ahe) |>
  mutate(R = rank(ahe)) |> #ragnerer dem etter hverandre
  mutate(R = R/length(ahe))

View(data)