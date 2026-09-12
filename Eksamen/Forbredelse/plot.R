
library(tidyverse)

boxplot <- data |>
  mutate(x_data = as.factor(x_data)) |> #må omgjøre til faktorer
  ggplot(aes(x = x_data, y = y_data)) +
  geom_boxplot(aes(group = x_data)) #grupperer etter faktorer


