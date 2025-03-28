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

##Replace 'heart<3' with 'heart'
ihtm_clean$favourite_shape <- gsub("heart <3", "heart", ihtm_clean$favourite_shape)

## Change all entries to lowercase
ihtm_clean$favourite_shape <- tolower(ihtm_clean$favourite_shape)
ihtm_clean$favourite_colour <- tolower(ihtm_clean$favourite_colour)

## Replace 'one' in favourite_number to numeric
word_to_num <- function(ihtm_clean) {
  recode(ihtm_clean,
         "one" = 1, "two" = 2, "three" = 3, "four" = 4, "five" = 5,
         "six" = 6, "seven" = 7, "eight" = 8, "nine" = 9, "ten" = 10,
         .default = as.numeric(ihtm_clean))  # Keeps existing numbers as they are
}

ihtm_clean$number_siblings <- word_to_num(ihtm_clean$number_siblings)
