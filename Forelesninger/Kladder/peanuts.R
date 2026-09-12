

library(rvest)
library(tidyverse)
library(janitor)
url  <- "https://en.wikipedia.org/wiki/R_(programming_language)"

tables <- read_html(url) |>
  html_table()

relase_codenames <- tables[[2]] |> 
  janitor::clean_names()

View(relase_codenames)