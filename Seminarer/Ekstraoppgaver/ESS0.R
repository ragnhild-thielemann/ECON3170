
library(tidyverse)

#vi har allerede datasettet lagret fra minnet
ESS9 <- read.csv("ESS9.csv")

ESS9_codebook <- read.csv("ESS9_codebook.csv")

ESS9_smaller <- ESS9 |>
  dplyr::select(c(cntry,agea, gndr, netustm)) |>
  mutate(netustm = replace_na(netustm,0)) 
  
tv_tid <- ESS9_smaller |>
  group_by(cntry)|>
  summarise(m = mean(netustm))
View(tv_tid)