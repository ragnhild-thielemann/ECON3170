
library(tidyverse)
ESS9 <- read.csv("ESS9.csv")

p <- ESS9 |>
  ggplot(data = ESS9, mapping = aes(x = agea, y = netustm)) + 
  geom_point() + 
  geom_smooth()


ESS9_average <- drop_na(ESS9)|>
  mutate(agea = as.numeric(agea)) |>
  
           
          
  mutate(agea = case_when(
                          agea %in% 18:30 ~ "18-30",
                          agea %in% 31:40 ~ "31-40",
                          agea %in% 0:18 ~ "0",
                          
                          agea %in% 41:50 ~ "41-50",
                          agea %in% 51:60 ~ "51-60",
                          agea > 60 ~ "60+"
    
  
    
  )) 


p <- ESS9_average |>
  ggplot(aes(x = agea, y = netustm)) + 
  geom_boxplot()
p
