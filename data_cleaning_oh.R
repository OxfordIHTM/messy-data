# Read and process occupational health data ------------------------------------

## Load libraries ----
library(openxlsx2)
library(dplyr)
library(tidyr)
library(ggplot2)


## Read occupational health data ----

rows <- list(6:23, 6:23, 6:23, 7:24, 10:27, 7:25, 7:25, 7:25, 7:25, 7:25, 7:25)
cols <- rep(list(c(1:5, 7:10)), 11)

occ_health <- Map(
  f = read_xlsx,
  file = "data/occupational_health.xlsx",
  sheet = 2:12,
  rows = rows,
  cols = cols,
  col_names = FALSE
) |>
  (\(x) { names(x) <- month.name[2:12]; x })() |>
  bind_rows(.id = "month") |>
  rename_with(
    .fn = function(x) {
      c(
        "month", "service", 
        paste("Male", c("15-24", "25-44", "45-64", "65+"), sep = "_"),
        paste("Female", c("15-24", "25-44", "45-64", "65+"), sep = "_")
      )
    }
  ) |>
  mutate(year = 2021, .after = "month")


## Process occupational health data ----

occ_health <- occ_health |>
  pivot_longer(
    cols = `Male_15-24`:`Female_65+`, 
    names_to = c("sex", "age_group"), values_to = "n", names_sep = "_"
  )
