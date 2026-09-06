

library(tidyverse)

draws = 10000

I = function(draws){
  u = runif(draws,0,1)
  
  g = function(x){
    return((x-1)/log(x))
    
  }
  return (mean(g(u)))
}


n_verdier = 10**(c(2,3,4,5,6,7))

tabell = tibble(N = n_verdier)
tabell <- tabell |> mutate(M = map_dbl(N,I)) |> mutate(D = M-log(2))
print(tabell)
ggplot(tabell) + geom_point(aes(x = N, y = D)) + scale_x_log10()

##

nytte = function(c){
  sigma = 2
  
  return(((c**(1-sigma))-1)/(1-sigma))
}

b = 5
e = exp(rnorm(1000,0,1))
c = b + e

print(mean(nytte(c)))