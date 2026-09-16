

library(tidyverse)

#konsumet i c_2 er en funksjon av konsumet i c_1
set.seed(141)
c_2 <- function(c_1, r = 0.05){

  c_2 <- (1 + r)*(Y-c_1)
  return (c_2)
  
}
    
U <- function(c_1,c_2, gamma = 0.3, beta = 0.96){
  
  U_ <- (c_1**(1-gamma))/(1-gamma) + beta*(c_2**(1-gamma))/(1-gamma)
  return(U_)
  
}

c1_values <- c(seq(1,10,0.1))

verdier <- tibble(c1 = c1_values, U = U(c1_values,c_2(c1_values)))



ggplot(verdier, aes(x = c1, y = U)) + geom_line() + labs(x = "C1", y = "Nytte", title = "Nytte som funskjon av c1")

c_max <- 0 ; nytte_max <-0
for (c in c1_values){
  nytte = U(c,c_2(c))
  if (nytte> nytte_max){
    nytte_max = nytte
    c_max = c
  }
}

sprintf("Den storste nytten oppnas med %0f som c1, og er da %0f", c_max, nytte_max)



MC <- function(c_1,N){
  total <- 0
  teller = 0
  for (n in 1:N){
    R = rexp(1,0.9)
    c_2 = c_2(c_1, r = R-1)
    total <- total + U(c_1,c_2)
   
  }
  print(teller)
  return(total/N)
}

plot(c1_values,MC(c1_values,100))

c_max <- 0 ; nytte_max <-0
for (c in c1_values){
  nytte = MC(c,1000)
  if (nytte> nytte_max){
    nytte_max = nytte
    c_max = c
  }
}

print(c(c_max,nytte_max))
