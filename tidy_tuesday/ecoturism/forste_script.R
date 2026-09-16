
library(ecotourism)
library(tidyverse)

reason <- ecotourism::tourism_reason

#gir oss navnet på de numeriske variablene i readson-scriptet. At de er gjort numeriske i reason, er for at det skal være murlig for maskinlæring å følge dem opp. 
name <- ecotourism::tourism_reason_name
View(reason)