# Read and process IHTM survey data --------------------------------------------

## Load libraries ----
library(openxlsx2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(digest)
library(countrycode)


## Read IHTM survey data ----
ihtm <- read_xlsx(file = "data/ihtm_2025.xlsx")


## Clean and process data ----
ihtm_clean <- ihtm |>
  filter(!is.na(name)) |>
  mutate(
    id = str_pad(1:length(id), width = 2, side = "left", pad = 0),
    name = lapply(name, FUN = digest) |> unlist(),
    college = str_remove_all(
      string = college, pattern = " College| college|\\."
    ) |>
      str_replace_all(pattern = "Hildas", replacement = "Hilda's") |>
      str_replace(pattern = "St  ", replacement = "St "),
    nationality = str_replace_all(
      string = nationality, pattern = "American", 
      replacement = "United States of America"
    ) |>
      countryname(destination = "country.name", warn = FALSE),
    country_residence = countryname(
      sourcevar = country_residence, destination = "country.name", warn = FALSE
    ),
    favourite_colour = case_when(
      str_detect(string = favourite_colour, pattern = "blue|Blue") ~ "blue",
      str_detect(string = favourite_colour, pattern = "orange|Orange") ~ "orange",
      str_detect(string = favourite_colour, pattern = "pink|Pink") ~ "pink",
      str_detect(string = favourite_colour, pattern = "grey|Grey|gray|Gray") ~ "grey",
      str_detect(string = favourite_colour, pattern = "purple|Purple") ~ "purple",
      .default = favourite_colour
    ),
    favourite_shape = case_when(
      str_detect(string = favourite_shape, pattern = "square|Square") ~ "square",
      str_detect(string = favourite_shape, pattern = "circle|Circle") ~ "circle",
      str_detect(string = favourite_shape, pattern = "heart|Heart") ~ "heart",
      str_detect(string = favourite_shape, pattern = "rectangle|Rectangle") ~ "rectangle",
      str_detect(string = favourite_shape, pattern = "start|Star") ~ "start",
      .default = favourite_shape
    ),
    number_siblings = ifelse(number_siblings == "one", 1, number_siblings) |>
      as.integer()
  )
