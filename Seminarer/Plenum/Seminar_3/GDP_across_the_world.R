#1
library(tidyverse)
library(docstring)
#leser csv-filen med et r-script
pwt <- read.csv("pwt.csv",header = TRUE, sep = ",")

#'Understrek gir oss en tibble, mens . gir oss en data-frame
#2 egen tibble med norges bnp
gdp_nor <- pwt |> 
  filter(countrycode == "NOR" & year<=2000 & year >= 1980) |>
  select(year, gdp) #velger de to variableen vi skal ha
  #' Det er flere måter å filtrer ut informasjonen vi ønsker oss. Between (year, 1980,2000)

#3 veksten i kina fra 1970 - 2000

pwt.china <- pwt |>
  filter(countrycode == "CHN") |> #filtrer som det første jeg gjør, slik databehandlignen bare gjøres for de aktuelle datapuntken. 
  arrange(year) |> #må ha årene i stigende rekkefølge, for å grantere at det blir time-series. Dette er nødvendig, da vi beregner for forrgje kolonne
  mutate(growth = (gdp - lag(gdp))/lag(gdp)) |>#lag tar forrgje verdi av gdp
  filter(between(year,1970,2000)) #må filtrer til slutt, for å ikke miste året 1969


#4 Skal lage en wide tabell over gdp i 1970 og 2000

pwt.growth <- pwt |> 
  mutate(lgdp = log(gdp)) |>
  filter(year==2000 | year == 1970) |>
  select(-countrycode)|>
  
  pivot_wider(
              names_from = year, #gjør at man kan få to variabler
              values_from = c(gdp,lgdp)) |>
  mutate(Vekst = (gdp_2000/gdp_1970)**(1/30)-1) 
#'formel for å regne gjennomsnittlig vekst over 30 år. Total vektst vil da være a**30. Trekker fra 1, for å finne postiv eller neagativ vekst

  

#5 lager et scatterplott
ggplot(pwt.growth) + geom_point(aes(x = log(gdp_1970),y = Vekst)) + geom_smooth(aes(x = lgdp_1970,y = Vekst)) + labs(x = "log(gdp1970)", y = "gjennomsnittlig vekst (1970-2000)", title = "Vekst mot bnp i 1970")

#6 lager en dummyvariabel for å finne antall år økonomien har vokst

dummy <- pwt |> 
  arrange(country,year)|> #da vi skal lage time-series, må vi sortere etter riktig antall år
  group_by(country)|> #grupperer landene sammen, og de er også sorteret etter år
  mutate(gdp_up = gdp > lag(gdp))|>  #lager en logisk vektor, som er true eller false
  summarize(frac_up = mean(gdp_up, na.rm = TRUE)) |>
  ungroup() #ungrupper for å hindre at alt går tilbake til grupperingene

View(dummy)

# skal nå merge denne tibblen sammen med det tidy-behandlete datasette jeg allerede har jobbet med

pwt.growth <- left_join(pwt.growth,dummy) |> 
  filter(Vekst>-1000) #fjerner ekstremverdiene

#7 Lager et scatterplott, og regner korrelasjonen mellom disse to variablene

ggplot(pwt.growth) + geom_point(aes(x = Vekst, y = Mean_dummy))

#finner korrelasjonen mellom disse to
c = cor(pwt.growth$Vekst,pwt.growth$Mean_dummy)

sprintf("Korrelajonen er %0f mellom gjennomsnittlig vekt, og jevn vekst",c)
