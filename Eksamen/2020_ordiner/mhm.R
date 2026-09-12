
library(tidyverse)
library(clock)

#Oppgave 1

#leser inn dataen som en tibbel
mobility <- read_csv("m.csv") |>
  group_by( date, sub_region_1)  #sorterer med hensyn på tid

sprintf("Datasettet gar fra %s til %s", min_date(mobility$date),max_date(mobility$date))

nasjonalt <- mobility |>
  mutate(nasjonal = as.factor(if_else(is.na(sub_region_1) == TRUE, 0,1))) |>
  filter(nasjonal == 0 | sub_region_1 == "Oslo")|>
  select()

View(nasjonalt)

oslo <- mobility |>
  filter (sub_region_1 == "Oslo") |>
  mutate(weekday = as_weekday(as.Date(date))) |>
  
  mutate(weekend = as.factor(if_else(weekday %in% c("Mon","Tue","Wed","Thu","Fri"),0,1))) |>
  
  mutate(weekday = as.factor(case_when(weekday %in% "Mon" ~ 1,
                             weekday %in% "Tue"~2,
                             weekday %in% "Wed"~3,
                             weekday %in% "Thu"~4,
                             weekday %in% "Fri"~5,
                             weekday %in% "Sat"~6,
                             weekday %in% "Sun"~7,
    
    
  )))|>
  rename(hjemmetid = residential_percent_change_from_baseline) |>
  select(weekend, weekday, hjemmetid,date) 



oslo_tid_hjemme <- oslo |>
  ggplot(aes(x = date, y = hjemmetid)) + 
  geom_line() + labs(x = "Tid (2020)" , y = "Prosentvis endring i tiden vi brukte hjemme")


ggplot(oslo, aes(x = date, y = hjemmetid, color = weekend)) + geom_line() + labs(title = "Hjemmetid i Oslo")

ggplot(oslo, aes( x = date, y = hjemmetid, color = weekday)) + geom_line() + labs(title = "Hjemmetid i Oslo")


#-----------------------------------------------------------------------------
#skal nå se på alle kommuner
#-----------------------------------------------------------------------------



alle_fylker <- mobility |>

  mutate(weekday = as_weekday(as.Date(date))) |>
  mutate(weekend = as.factor(if_else(weekday %in% c("Mon","Tue","Wed","Thu","Fri"),0,1))) 

ukedag <- alle_fylker |>

  filter(weekend == 0)|>
  drop_na(sub_region_1) |>
  mutate(o = as.factor(if_else(sub_region_1 == "Oslo",0,1))) 

ggplot(ukedag, aes(x = date, y = residential_percent_change_from_baseline, color = o)) + geom_line()


total_alle_fylker <- alle_fylker |>
  drop_na(sub_region_1) |>
  mutate(sub_region_1 = as.factor(sub_region_1))|>
  rename(hjemmetid = residential_percent_change_from_baseline) |>
  select(hjemmetid, date, sub_region_1)|>
  summarize(gjennom = mean(hjemmetid, na.rm = TRUE), med = median(hjemmetid, na.rm = TRUE))


tot <- total_alle_fylker |>
  group_by(sub_region_1) |>
  summarise(m = mean(gjennom, na.rm = TRUE), me = mean(med, na.rm = TRUE))
View(tot)
ggplot(total_alle_fylker, aes(x = date, y = gjennom, color = sub_region_1)) + geom_line()



View(total_alle_fylker)
