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


### Class exercise Mar 28, 2025
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

## Cleaning and Reading cyclones dataset ----

for (i in 2:6) {
    assign(
      paste("sheet", i, sep = ""),
      read_xlsx(path = "data/cyclones.xlsx", sheet = i)
    )
  }


cyclones <- Map(
  f = read_xlsx,
  path = "data/cyclones.xlsx",
  sheet = 2:6,
  col_names = TRUE
) |>
  bind_rows()

##Binding tables -------

cyclones_sum <- bind_rows(sheet2, sheet3, sheet4, sheet5, sheet6) 

###Summary table package -----
table(cyclones$category_name)
table(cyclones$category_name, cyclones$year)



###plotting ----
library(oxthema)
library(RColorBrewer)
?brewer.pal
cyclones |>
  # count(category_name, year) |>
  #ggplot(aes(x = year, y = n, fill = category_name)) +
  ggplot(aes(x = year, y = speed, group = category_name)) +
  # geom_bar(position = "stack", stat = "identity") +
  geom_violin(colour = "red", fill = "#4b876e", alpha = 0.3) +
  geom_jitter(colour = "royalblue", size = 3, width = 0.2)
























































