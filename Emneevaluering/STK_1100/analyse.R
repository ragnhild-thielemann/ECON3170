
library(tidyverse)
data = read.csv("STK1100.csv", sep = ";")[,-c(1,2)] 

organisering <- data[,-c(1,2)] |> select(X:X.6) 

colnames(organisering) = c("Intrykk","Vannsklighet","Arbeidsmengde","Timer_brukt","Informasjon fra foreleser","To forelesere","Forskjell i undervisning")

View(organisering)

View(data)

a = colnames(data[14])
b = colnames(data[18])
print(a)

lering <- data |> select(a:b) 


colnames(lering) = c("Forelesning","Gruppetime","Ukesoppgaver","Eksamensoppgaver","Semestersider")

lering <- lering |> mutate()
print(unique(lering$Forelesning))
s = lering$Forelesning
a  = case_when(s == "Alltid" ~5,
               s == "Ofte " ~4, 
               s == "Av og til" ~3,
               s == "Sjelden" ~2, 
               s == "Aldri " ~1,
               
)
print(a)
lering$Forelesning = a
View(lering)