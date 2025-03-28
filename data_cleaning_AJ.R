# Examples of data cleaning RWD ------------------------------------------------

## Load libraries ----
library(openxlsx2)
library(dplyr)
library(tidyr)
library(ggplot2)

## Read data sets needed ----
sheet1 <- read_xlsx(file = "data/pop_death.xlsx", sheet = 1, start_row = 2)
sheet2 <- read_xlsx(file = "data/pop_death.xlsx", sheet = 2, start_row = 2)

## Other option for reading data sets
for (i in 1:2){
  assign(
    paste("sheet", i, sep = ""), read_xlsx(file = "data/pop_death.xlsx", sheet = i, start_row = 2)
  )
}


## Clean and process the pop and death data ----

### Rename sheet column labels for rbinding ----
sheet1 <- sheet1 |>
  rename(age = Age) |>
  rename_with(
    .fn = function(x) sub(pattern = "M_", replacement = "", x = x),
    .cols = `2011_M_Pop`:`2021_M_Deaths`
  )
names(sheet1)

sheet2 <- sheet2 |>
  rename(age = Age) |>
  rename_with(
    .fn = function(x) sub(pattern = "F_", replacement = "", x = x),
    .cols = `2011_F_Pop`:`2021_F_Deaths`
  )
names(sheet2)

### Check that sheet1 and sheet2 are same names
identical(names(sheet1), names(sheet2))

### Row bind sheet1 and sheet2
pop_death <- bind_rows(sheet1, sheet2, .id = "sex") |>
  mutate(sex = ifelse(sex == 1, "Male", "Female"))

pop <- pop_death |>
  select(-paste(2011:2021, "Deaths", sep ="_")) |>
  pivot_longer(
    cols = contains("Pop"),
    names_to = "year", values_to = "pop"
    ) |>
  mutate(year = sub(pattern = "_Pop", replacement = "", year))

death <- pop_death |>
  select(-paste(2011:2021, "Pop", sep ="_")) |>
  pivot_longer(
    cols = contains("Deaths"),
    names_to = "year", values_to = "death"
    ) |>
mutate(year = sub(pattern = "_Deaths", replacement = "", year))

pop_death <- left_join(pop, death)





































































