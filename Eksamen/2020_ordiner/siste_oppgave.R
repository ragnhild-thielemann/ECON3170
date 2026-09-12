library(tidyverse)

#leser inn dataen som en tibbel
mobility <- read_csv("m.csv")   #sorterer med hensyn på tid

sprintf("Datasettet gar fra %s til %s", min_date(mobility$date),max_date(mobility$date))

oslo <- mobility|>
  filter(sub_region_1 == "Oslo") |>
  rename(hjemme_oslo = residential_percent_change_from_baseline) |>
  select(date, hjemme_oslo) 
  


nasjonalt <- mobility |>
  mutate(nasjonal = as.factor(if_else(is.na(sub_region_1) == TRUE, 0,1))) |>
  filter(nasjonal == 0) |>
  rename(hjemme = residential_percent_change_from_baseline) |>
  select(date, hjemme) |>
  left_join(oslo)
 


View(nasjonalt)