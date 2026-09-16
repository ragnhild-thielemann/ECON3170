

library(tidyverse)
data <- read.table("tekst.txt")|>
  select(V6,V7)|>
  rename(x = V6, s = V7)
J = 4
I = 5
m = mean(data$x)

mellom = sum((data$x-m)**2)*J
innad = sum(data$s**2*(J-1))
print(c(mellom, innad))
a = ((mellom/(I-1))/(innad/(I*(J-1))))
print(a)

b = pf(a,I-1,(J-1)*I)
print(1-b)