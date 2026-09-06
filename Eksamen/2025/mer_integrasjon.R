

library(tidyverse)
Y = 10
r = 0.05
R = rexp(10,0.9)
U = function(c1){
  gamma = 0.4
  beta = 0.96
  
  R_mean = mean(R)
  c2 = R_mean*(Y-c1)
  
  svar = ((c1**(1-gamma))/(1-gamma) + beta*((c2**(1-gamma))/(1-gamma)))
  return (svar)
  }

c1_values = seq(1,Y,0.1)

U_values = U(c1_values)

ggplot() + geom_line(aes(x = c1_values, y = U_values))

i = 1
maks_U = U(1)
mask_c = 1
for (c in c1_values){
  if (U(c)>maks_U){
    maks_U = U(c)
    maks_c = c
  }
}
print(c(maks_U,maks_c))


