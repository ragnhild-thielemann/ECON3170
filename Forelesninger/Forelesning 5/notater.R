
library(tidyverse)
library(bit)
x <- 17
basis <- 2:x

a <- x

for (b in basis){
  a <- x
  a_verdier = c()
  while(a>0){
    a_verdier <- c(a_verdier,a%%b)
    a <- a%/% b
    
  }
  a_verdier <- reverse_vector(a_verdier)
  print(c(b,(a_verdier)))
}
