

rm(pi) 
library(docstring)
library(microbenchmark) #bibliotek for å ta tiden på noe
G_funk = function(terms){
  pi = 0
  for (n in 0:terms){
    ledd = 4/(2*n + 1) * (-1)**n
    pi = pi + ledd
  }
  return (pi)
}

N_funk= function(terms){
  pi = 3
  for (n in 1:terms){
    k = 2*n
    pi = pi + 4/(k * (k + 1) * (k + 2)) * (-1)**(n+1)
    
  }
  return (pi)
}


terms <- 10**c(1,2,3,4,5,6)
n_runs <- length(terms)
pi_gl <- numeric(n_runs) #lager nullvektoren med lengden 0
pi_no <- numeric(n_runs) 


for (i in 1:n_runs){
  pi_gl[i] <- G_funk(terms[i]) #indekser oss gjennom vektoren
  pi_no[i] <- N_funk(terms[i])
}


pi_approx <- tibble(N = terms, GK = pi_gl, NK = pi_no) |>
  mutate(error_GK = abs(GK-pi),
         error_NK = abs(NK-pi)) |>
  pivot_longer(cols = c("error_NK", "error_GK"),
    names_prefix = "error_",
               names_to = "Type",
               values_to = "Error") |>
  select(N, Type, Error)

ggplot(pi_approx) + geom_point(aes(x = N, y = Error, color = Type)) + geom_line(aes(x = N, y = Error, color = Type)) + scale_x_log10() + scale_y_log10() 