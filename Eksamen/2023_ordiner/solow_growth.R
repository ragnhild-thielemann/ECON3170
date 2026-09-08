
library(tidyverse)
library(docstring)
#parametere til å starte modellen



k_ss = ((s*A)/delta)**(1/(1-alpha))

sprintf("Likevekten i kapital per capita er %0f",k_ss)
k_t = function(k){
  A = 10
  alpha = 0.3
  s = 0.2
  delta = 0.1 #har disse som indre variabler
  return (s*A*k**alpha + (1-delta)*k)
}


k_0 = 1 ; k_1 = 5
tid = 10000
k0_vektor = c(k_0) ; k1_vektor = c(k_1)
for (t in 2:tid){
  k_0 = k_t(k_0)
  k_1 = k_t (k_1)
  k1_vektor = c(k1_vektor,k_1)
  k0_vektor = c(k0_vektor,k_0)
}




f <- tibble(t = 1:tid,K_0 = k0_vektor, K_1 = k1_vektor)
View(f)

f_man = f |> pivot_longer(cols = c("K_0","K_1"),
                          names_prefix = "K_",
                          names_to = "Startverdi",
                          values_to = "value")

ggplot(f_man) + geom_line(aes(x = t, y = value, color = Startverdi)) 

#' Klarer jeg å lage en modell som gjør begge kalkulasjonene

#skal teste inn for ulike verdier av s

k_t_s = function(k,s){
  A = 10
  alpha = 0.3
  
  delta = 0.1
  return (s*A*k**alpha + (1-delta)*k)
}

#lager en løkke, for å lage en tibble
s_verdier = seq(0,0.5,0.1)
Tid = 100
ks = tibble(t = 1:Tid)
View(ks)
for (s in s_verdier){
  k = 1
  print(s)
  s_vektor = c(k)
  for (t in 2:Tid){
    k = k_t_s(k,s)
    s_vektor = c(s_vektor,k)
  }
 
  ks = bind_cols(ks,tibble(s_vektor)) #legger kolonnene etter hverandre
  
}

names(ks)
ks <- ks |> pivot_longer(col = names(ks)[-1],
                   names_prefix = "s_vektor..",
                   names_to = "verdi",
                   values_to = "japp")
View(ks)

ggplot(ks) + geom_line(aes(x = t, y = japp,color = verdi))