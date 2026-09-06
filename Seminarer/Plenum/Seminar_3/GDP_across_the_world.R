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

dummy <- pwt |> 
  mutate (dummy = if_else(gdp<lag(gdp),0,1)) |>
  summarise(Mean_dummy = mean(dummy,na.rm = TRUE),.by = country)

# skal nå merge denne tibblen sammen med det tidy-behandlete datasette jeg allerede har jobbet med

pwt.growth <- left_join(pwt.growth,dummy) |> 
  filter(Vekst>-1000) #fjerner ekstremverdiene

#7 Lager et scatterplott, og regner korrelasjonen mellom disse to variablene

ggplot(pwt.growth) + geom_point(aes(x = Vekst, y = Mean_dummy))

#finner korrelasjonen mellom disse to
c = cor(pwt.growth$Vekst,pwt.growth$Mean_dummy)

sprintf("Korrelajonen er %0f mellom gjennomsnittlig vekt, og jevn vekst",c)
