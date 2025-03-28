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
      str_replace_all(pattern = "Hildas", replacement = "Hilda's"),
    nationality = countryname(
      sourcevar = nationality, destination = "country.name"
    ) |>
      (\(x) ifelse(is.na(x), "United States of America", x))(),
    country_residence = countryname(
      sourcevar = country_residence, destination = "country.name"
    )
  )
