

library(tidyverse)
library(haven)
library(recipes)

#leser stata-filen med haven fra tidyverse
gss <- haven::read_dta("GSS2018.dta") |>
  select(conlegis,trust,age,region)|> #fitrerer ut de relevante kolonnene for oppgave
  mutate(trust = replace_na(trust, 0))
colnames(gss)
#---------------------------------------------
#Antallet som svarte på spørsmålet om tilitten til 
#kongressen
#---------------------------------------------


tibbleconlegis <- gss |> #henter datasettet
  drop_na(conlegis)  #dropper de kolonnene med manglende observasjoner i den gitte variablen
  
antall_conlegis <- nrow(tibbleconlegis)

sprintf("Det var %s som ble spurt om tiliten til kongressen", antall_conlegis)

conlegis_plot_bar <- tibbleconlegis |>
  ggplot(aes(x = conlegis)) + 
  geom_bar() + 
  labs(x = "tilitt", y = "antall", title = "Tilitt til myndighetene")


conlegis_plot_scatter <- tibbleconlegis|>
  mutate(noconlegis = as.factor(if_else(conlegis %in% c(3),1,0))) |> #må gjøres til en faktor, ikke en kontinuerlig variabel. Den er diskret
  ggplot(aes( x = noconlegis, y =age)) + 
  geom_boxplot(aes(group = noconlegis)) + 
  labs(x = "tilitt til myndigheten", y = "Alder") + 
  scale_x_discrete(labels = c("0" = "Noe eller mye ",  "1" = "Lite")) #bytter navn på aksetitlene

t <- tibbleconlegis |>
  mutate(trust = as.factor(trust)) |>
  mutate(notrust = if_else(trust %in% c(2),1,0)) |>
  mutate(noconlegis = (if_else(conlegis == 2,1,0))) |> #må gjøres til en faktor, ikke en kontinuerlig variabel. Den er diskret
  drop_na(notrust,noconlegis) 


doses <- read.csv("doses.csv") |>
  mutate(division = case_when(division == "Pacific" ~ 9,
                              division == "New England" ~ 1,
                              division == "Middle Atlantic" ~ 2,
                              division == "East North Central" ~ 3,
                              division == "West North Central" ~ 4,
                              division == "South Atlantic" ~ 5,
                              division == "East South Central" ~ 6,
                              division == "West South Central" ~ 7,
                              division == "Mountain" ~ 8,
                              )) 

vak <- doses |>
  group_by(division)|>

  summarise(andel = (sum(doses)/sum(population)))

trust_sum <- tibbleconlegis |>
  mutate(noconlegis = (if_else(conlegis == 2,1,0))) |> #må gjøres til en faktor, ikke en kontinuerlig variabel. Den er diskret
  group_by(region) |>
  summarise(total_trust = mean(noconlegis)) |>
  select(region, total_trust) |>
  rename(division = region) |>
  left_join(vak) |>
  ggplot(aes(x = total_trust, y = andel )) + 
  geom_smooth()
  

trust_sum







  

