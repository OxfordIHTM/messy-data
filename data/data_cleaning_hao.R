# Examples of data cleaning of RMD -----------------

## Load Libraries
library(openxlsx2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(gtsummary)
#library(tidyverse)

## Read datasets needed

sheet1 <- read_xlsx(file = "data/pop_death.xlsx", sheet = 1, start_row = 2)
sheet2 <- read_xlsx(file = "data/pop_death.xlsx", sheet = 2, start_row = 2)

## Loop
#for(i in 1:2){
#  assign(
#    paste0("sheet", i), read_xlsx(file = "data/pop_death.xlsx", sheet = i, sheet_row = 2)
#  )
#}

## Rename sheet columns labels
sheet1 <- sheet1 %>%
  rename(age = Age) %>% 
  rename_with(.fn = function(x) sub(pattern = "M_", replacement = "", x=x),
              .cols = '2011_M_Pop':'2021_M_Deaths'
                                 )
#sheet2 <- sheet2 %>%
#  rename(age = Age) %>% 
#  rename_with(.fn = function(x) sub(pattern = "F_", replacement = "", x=x)
#  )

sheet2 <- sheet2 %>%
  rename(age = Age) %>% 
  rename_with(.fn = function(x) sub(pattern = "F_", replacement = "", x=x),
              .cols = '2011_F_Pop':'2021_F_Deaths'
  )

identical(names(sheet1), names(sheet2))

## Clean and process the pop and death data
#pop_death <- rbind(sheet1, sheet2)

pop_death <- sheet1 %>% bind_rows(sheet2, .id = "sex") %>% 
  mutate(sex = ifelse(sex == 1, "Male", "Female"))



pop_death_1 <- pop_death %>%
  select(-paste(2011:2021, "Deaths", sep ="_")) %>% 
  pivot_longer(
    cols = contains("Pop"),
    names_to = "year",
    values_to = "pop") %>% 
  mutate (year = str_replace(year,"_Pop", ""))

pop_death_2 <- pop_death %>% 
  select(-paste(2011:2021, "Pop", sep ="_")) %>% 
  pivot_longer(
    cols = contains("Deaths"),
    names_to = "year",
    values_to = "death") %>% 
  mutate (year = str_replace(year,"_Deaths", ""))

#pop_death_2 <- pop_death %>% 
#  select(-paste(2011:2021, "Pop", sep ="_")) %>% 
#  pivot_longer(
#    cols = contains("Deaths"),
#    names_to = "year",
#    values_to = "death") %>% 
#  mutate (year = sub(pattern = "_Deaths", replacement ="", year))

pop_death <- left_join(pop_death_1, pop_death_2) %>% 
  mutate(year = as.numeric(year))

is.numeric(pop_death$year)

tbl_summary(pop_death)

