

library(readxl)
library(docstring)
s <- read_xlsx("subscribers (1).xlsx") |>
  mutate(Start = ymd(as.Date(Start)), End = ymd(as.Date(End))) |>
  mutate(Intervall = interval(Start, End)) 


#' må finne antallet som abonerer 15 april
#' 


april <- s |>

  filter(ymd("2024-04-15")%within%  Intervall)


I= seq(ymd("2024-01-01"), length.out = 365, by ="1 day")
print(I)
brod_vektor = c()

for (i in 1:365){
  brod <- s |>
    filter (I[i] %within% Intervall)
  antall <- sum(brod$Number)
  brod_vektor = c(brod_vektor,antall)
  
}

t = tibble(time = I, brod = brod_vektor)

ggplot(t, aes(x = time, y = brod)) + geom_line()
