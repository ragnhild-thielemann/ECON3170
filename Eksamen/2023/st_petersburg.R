

library(tidyverse)
draws = 10000

I = function(draws){
  u = runif(draws,0,1)
  
  g = function(x){
    return((x-1)/log(x))
    
  }
  return (mean(g(u)))
}


n_verdier = 10**(c(1,2,3,4,5))

tabell = tibble(N = n_verdier,M = I(n_verdier), D = I(n_verdier)-log(2))
View(tabell)