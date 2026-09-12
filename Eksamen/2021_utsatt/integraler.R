
library(tidyverse)
set.seed(412)
print(pi)
f = function(x){
  return ((2*x + 5)/(x**2 + 1))
}



MC <- function(f,N){
  r = c()
  
  for (n in N){
    x_verdier = runif(n)
    r = c(r, mean(f(x_verdier)))
    
  }
  return(r)
}

n = 1000

correct = (5*pi)/4 + log(2)


t = tibble(N = draws, MC = MC(f,draws), MC_error =MC(f,draws)-correct )


ggplot(t,aes(x = N, y = MC_error)) + 
  geom_line() + 
  scale_x_log10() 
