
library(tidyverse)

vdem <- read_csv("vdem.csv")

start_ar <- year(min(vdem$historical_date)) 
slutt_ar <- year(max(vdem$historical_date))

sprintf("Det strekker seg fra %g til %g", start_ar, slutt_ar)

land_2021 <- vdem |>
  filter(year==2021) |>
  nrow()


sprintf("Det var %g land i 2021", land_2021)

kvantiler <- vdem |>
  select(year,country_name,v2x_libdem) |>
  filter(year>1950) |>
  mutate(v2x_libdem = replace_na(v2x_libdem,0))|>
  summarise(q_50 = quantile(v2x_libdem,0.5),q_25 = quantile(v2x_libdem,0.25), q_75 = quantile(v2x_libdem,0.75), .by = year) |>
  pivot_longer(cols = -year,
               names_prefix = "q_",
               names_to = "Kvantil",
               values_to = "Indeks") |>
  ggplot(aes(x = year, y = Indeks, color = Kvantil)) + 
  geom_line() + 
  labs(x = "Year", y ="Indeks", title = "Utviklingen i demokrati")

kvantiler

vdem_wide <- vdem |>
  filter(year %in% seq(1980,2020,10))|>
  select(country_name, year,v2x_libdem)|>
  pivot_wider(names_prefix = "demo_",
              names_from = year,
              values_from = v2x_libdem)


View(vdem_wide)


library(readxl)

fh <- read_xlsx("FreedomHouse.xlsx")

names(fh) <- c("Land", "score_staus", "polrigths", "civilrights")

fh <- fh |>
  mutate(Status = word(score_staus,2,-1),
    Score = as.numeric(word(score_staus,1))
         )|>
  mutate(Status = as.factor(Status)) |>
  ggplot(aes(x = Status, y = Score)) + 
  geom_boxplot(aes(group = Status)) + 

  scale_x_discrete(limits  = c("Not Free", "Partly Free", "Free")) + 
  theme_bw()


fh


