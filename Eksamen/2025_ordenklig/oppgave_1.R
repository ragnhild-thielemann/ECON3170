
library(tidyverse)
library(docstring)


rader <- nrow(norad)
prosjekter <- length(unique(norad$agreement_title))

prosjekter_per_land <- norad |>
  group_by(recipient_country) |>
  summarise(p = n()) #teller over antall ganger hvert land kommer

View(prosjekter_per_land)

sprintf("Det er %d rader i datasettet, og %d unike prosjekter",rader, prosjekter)

#'[1] "Det er 29522 rader i datasettet, og 7897 unike prosjekter"
#----------------------------------------------------------------------

#lager en tibble med bare de prosjektene som har postiv støtte
class(norad$disbursements_1000_nok)
positive <- norad|>
  filter(as.numeric(disbursements_1000_nok) > 0)

negative_verdier = rader - nrow(positive) 

sprintf("Det er %0f rader med neative støtte", negative_verdier)

