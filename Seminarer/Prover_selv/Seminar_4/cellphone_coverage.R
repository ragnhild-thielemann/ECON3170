
library(jsonlite) #pakke for å konverterer javascripts inn i normalt tekstformat
library(tidyverse)
library(sf)
library(terra)
library(exactextractr)

zaf <- fromJSON("provinces.geojson")

tibble(region = names(zaf), data = zaf) |>
  unnest_longer(data) |>
  unnest_wider(data) |>
  mutate