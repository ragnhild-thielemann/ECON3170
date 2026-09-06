library(tidyverse)


data <- read_csv("wwbi_data.csv")
codebook <- read_csv("wwbi_series.csv")


data_demo <- data |> filter(country_code == "NOR" & indicator_code == "BI.EMP.FRML.PB.ZS")

for (i in 1:nrow(data_demo)){
  print(sprintf("Det var %0g av arbeidstokken i offentlig sektor i %s",data_demo[i,4]*100, data_demo[i,3] ))
 
}


data_demo_1 <- data |> filter(indicator_code ==  "BI.EMP.FRML.PB.ZS") |> summarise(m = mean(value), .by = year)

View(data_demo_1)