# Read and process population and death data -----------------------------------

## Load libraries ----
library(openxlsx2)
library(dplyr)
library(tidyr)
library(ggplot2)
#library(tidyverse)


## Read datasets needed ----

### Read sheet for males and females separately ----
sheet1 <- read_xlsx(file = "data/pop_death.xlsx", sheet = 1, start_row = 2)
sheet2 <- read_xlsx(file = "data/pop_death.xlsx", sheet = 2, start_row = 2)

### Alternative code to loop through the sheets to read ----
# for (i in 1:2) {
#   assign(
#     paste("sheet", i, sep = ""), 
#     read_xlsx(file = "data/pop_death.xlsx", sheet = i, start_row = 2)
#   )
# }


## Clean and process the pop and death data ----

### Rename sheet columns labels for rbinding ----
sheet1 <- sheet1 |>
  rename(age = Age) |>
  rename_with(
    .fn = function(x) sub(pattern = "M_", replacement = "", x = x),
    .cols = `2011_M_Pop`:`2021_M_Deaths`
  )

sheet2 <- sheet2 |>
  rename(age = Age) |>
  rename_with(
    .fn = function(x) sub(pattern = "F_", replacement = "", x = x),
    .cols = `2011_F_Pop`:`2021_F_Deaths`
  )

### Check that sheet1 and sheet2 has same names ----
identical(names(sheet1), names(sheet2))

### row bind sheet1 and sheet2 and then pivot longer ----
pop_death <- bind_rows(sheet1, sheet2, .id = "sex") |>
  mutate(sex = ifelse(sex == 1, "Male", "Female"))

### Alternative pivot longer approach that can replace lines below ----
pop_death <- pop_death |>
  pivot_longer(
    cols = `2011_Pop`:`2021_Deaths`, 
    names_to = c("year", ".value"), 
    names_sep = "_"
  ) |>
  rename(pop = Pop, death = Deaths) |>
  relocate(year, .before = sex) |>
  arrange(sex, year)

### Pivot longer approach initially designed in class ----
# pop <- pop_death |>
#   select(-paste(2011:2021, "Deaths", sep = "_")) |>
#   pivot_longer(
#     cols = contains("Pop"),
#     names_to = "year", values_to = "pop"
#   ) |>
#   mutate(year = sub(pattern = "_Pop", replacement = "", year))
# 
# death <- pop_death |>
#   select(-paste(2011:2021, "Pop", sep = "_")) |>
#   pivot_longer(
#     cols = contains("Deaths"),
#     names_to = "year", values_to = "death"
#   ) |>
#   mutate(year = sub(pattern = "_Deaths", replacement = "", year))
# 
# pop_death <- left_join(pop, death) |>
#   relocate(year, .before = sex) |>
#   arrange(year)

## Lifetable analysis ----

### plot lifetables

rbind(
  pop_death,
  pop_death |>
    summarise(
      total_pop = sum(pop), total_death = sum(death), .by = c(year, age)
    ) |>
    pivot_longer(
      total_pop:total_death, names_to = c("sex", ".value"), names_sep = "_"
    ) |>
    relocate(sex, .before = age) |>
    mutate(sex = ifelse(sex == "total", "Total"))
) |>
  mutate(death_rate = death / pop) |>
  ggplot(mapping = aes(x = age, y = death_rate, colour = year)) +
  geom_smooth(method = "gam", se = FALSE) +
  scale_x_continuous(
    breaks = seq(from = 0, to = 85, by = 10), limits = c(0, 85)
  ) +
  scale_y_continuous(
    breaks = seq(from = 0, to = 0.3, by = 0.05), limits = c(0, 0.3)
  ) +
  facet_wrap(. ~ sex, ncol = 5) +
  theme_oxford(grid = "XxYy") +
  theme(
    panel.border = element_rect(
      colour = get_oxford_colour("Oxford blue"), fill = NA, linewidth = 1
    ),
    legend.position = "top"
  )




