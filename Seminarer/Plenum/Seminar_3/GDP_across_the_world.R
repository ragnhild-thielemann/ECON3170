#1
library(tidyverse)
#leser csv-filen med et r-script
pwt <- read.csv("pwt.csv",header = TRUE, sep = ",")


#2 egen tibble med norges bnp
gdp_nor <- pwt |> 
  filter(countrycode == "NOR" & year<=2000 & year >= 1980)

#3 veksten i kina fra 1970 - 2000

pwt.china <- pwt |>
  filter(countrycode == "CHN" & year<=2000 & year >= 1980) |> #filtrer som det første jeg gjør, slik databehandlignen bare gjøres for de aktuelle datapuntken 
  mutate(growth = (gdp - lag(gdp))/lag(gdp)) #lag tar forrgje verdi av gdp

#4 Skal lage en wide tabell over gdp i 1970 og 2000

pwt.growth <- pwt |> 
  select(-countrycode)|>
  filter(year==2000 | year == 1970) |>
  pivot_wider(names_prefix = "gdp",
              names_from = year,
              values_from = gdp) |>
  mutate(Vekst = (gdp2000-gdp1970)/(30)) 

#5 lager et scatterplott
ggplot(pwt.growth) + geom_point(aes(x = log(gdp1970),y = Vekst)) + labs(x = "log(gdp1970)", y = "gjennomsnittlig vekst (1970-2000)", title = "Vekst mot bnp i 1970")

#6 lager en dummyvariabel for å finne antall år økonomien har vokst


