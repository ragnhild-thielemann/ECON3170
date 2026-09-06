
library(docstring) #importerer et biblitek for docstrings (wtf, R er jo bare tull, jeg importere jo bare hele python)
library(tidyverse)
#1 Finne inversfunksjon og likevekt


price <- function(stock){
  #'Vi har fått oppgitt at tilbudet er lik kvantum av kjøtt
  #'da det ikke kan lagres.
  #'Vi har også etterspørselsfunksjonen D(p) = 100-p. 
  #'Dermed blir prisen, som fuksjon av  kvantum, alstå den inverse av denne
  return(100-stock) }

stock = 50
likevekt = price(stock)

sprintf("Nar tilbudet er %s, er prisen %s",stock, likevekt)

#2 

tilbud = function(pris){
  #' Vi har at tilbudt kvantum er en funksjon av prisen foregående år. 
  #' Dette gir griseparadokset. 
  c = 1 #en parameter for gevinsten ved å øke produksjon
  return (pris/c)
}

tid = 1:10 #går over 10 perioder
x1 = 30 #starter med 30 griser

kvantum_griser = c()
for (t in tid){
  kvantum_griser = c(kvantum_griser, x1) #legger det innn i vektoren
  
  pris = price(x1) #finner prisen ved x1 antall griser
  x1 = tilbud(pris) #finner hvor mye bonden da produserer neste år
}

print(length(kvantum_griser))
print(length(tid))
ggplot() + geom_point(aes(x = tid, y = kvantum_griser)) + labs(x = "Tid", y = "Antall griser", title = "Griseparadokset")

#4 ny bonde som finner gjennomsnittet

Tid = 500 #går over 10 perioder
x1 = 30  ; x2 = 40 

kvantum_griser = c(x1,x2)
for (t in 3:Tid){
  sjokk = rbernoulli(1,0.02)*20
  x1 = kvantum_griser[length(kvantum_griser)-1] ; x2 = kvantum_griser[length(kvantum_griser)]
  bondens_regnskap = (x1+x2)/2
  pris = price(bondens_regnskap) #finner prisen ved x1 antall griser
  x3 = tilbud(pris) + sjokk #finner hvor mye bonden da produserer neste år
  kvantum_griser = c(kvantum_griser, x3) #legger det innn i vektoren
  
  }


print(length(kvantum_griser))

ggplot() + geom_line(aes(x = 1:Tid, y = kvantum_griser)) + labs(x = "Tid", y = "Antall griser", title = "Griseparadokset med sjokk")

#Griseparadokset med sjokk

