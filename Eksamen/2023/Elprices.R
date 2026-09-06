
#imporerer bibliotek for å lese exelfiler
library(readxl)
library(tidyverse)
#importerer datasett over strømprisene
temp = read_csv("temperatures.csv") 
temp <- temp |> arrange(desc(day))
Elprices <- read_excel("Elprices.xlsx")
El_des <- Elprices #bevarer datasettet for bare desember
for (m in 1:11){ #løper gjennom alle måneder, og legger radene i bunnen av datasettet
  M = month.name[m]
  sheet = read_excel("Elprices.xlsx", sheet = M)
  
  
  Elprices = bind_rows(Elprices, sheet)
}

View(Elprices)
#Lager et nytt datasett med bare de norske prisene

Elprices_norge <- Elprices |> select(c(date,no1:no4)) 

ggplot(Elprices_norge) + geom_line(mapping = aes(x = date, y = no1)) + labs(x = "Date", y = "Pris", title = "Strompris i prissone 1(2023)")

#gjør det om til et korrekt tidy format, slik at vi har  en observasjon per pris
long = pivot_longer(Elprices_norge, cols = c(names(Elprices_norge)[-1]),
                      names_prefix = "no",
                      names_to = "prissone",
                      values_to = "pris")


ggplot(long) + geom_line(aes(x = date, y = pris, color = prissone)) + labs(x = "Dato", y= "Pris", title = "Pris i alle prissonene (2023)")
View(long)
#skal importere for alle måneder i løpet av året
View(long)

a = long |> summarise(maks = max(pris), .by = c(prissone))

El_des <- El_des |> select(c("date","no1")) |> bind_cols(temp) 
View(El_des)
ggplot(El_des) + geom_point(aes(x = no1, y = temp)) + geom_smooth(aes(x = no1, y = temp))
cor(El_des$no1, El_des$temp)