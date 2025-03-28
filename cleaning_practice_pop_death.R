# Example of data cleaning of RWD: pop_death dataset----


# Load libraries ----
library(openxlsx2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
#library(tidyverse)


## Read datasets needed ----
sheet1 <- read_xlsx(file = "data/pop_death.xlsx", sheet = 1, start_row = 2)
sheet2 <- read_xlsx(file = "data/pop_death.xlsx", sheet = 2, start_row = 2)

## An alternative to lines 14,15 ----
## for when there are multiple sheets, use one command to perform same task as above ----
#for (i in 1:2) {
  #assign(
    #paste0("sheet", i), read_xlsx(file = "data/pop_death.xlsx", sheet = 1, start_row = 2))}


## Clean and process the pop and death data ----
#rbind(sheet1, sheet2) --> rbind() allows you to bind together vertically two sheets

## Rename sheet column labels for rbinding ----
sheet1 <- sheet1 %>% 
  rename(age = Age) %>%
  rename_with(
    .fn = function(x) sub(pattern = "M_", replacement = "", x = x),
    .cols = `2011_M_Pop`:`2021_M_Deaths`
  )

sheet2 <- sheet2 %>% 
  rename(age = Age) %>%
  rename_with(
    .fn = function(x) sub(pattern = "F_", replacement = "", x = x),
    .cols = `2011_F_Pop`:`2021_F_Deaths`
  )

## Check that sheet1 and sheet2 have same names ----
identical(names(sheet1), names(sheet2))

## Row bind sheet1 and sheet2 ----
pop_death <- bind_rows(sheet1, sheet2, .id = "sex") %>%
  mutate(sex = ifelse(sex == 1, "Male", "Female"))

## Pivot longer ----
#pop <- pop_death %>%
#  select(-paste(2011:2021, "Deaths", sep = "_")) %>%
#  pivot_longer(
#    cols = contains("Pop"),
#    names_to = "year", values_to = "pop"
#    )%>%
#  mutate(year = str_replace(year, "_Pop", ""))
##mutate(year = sub(pattern = "_Pop", replacement = "", year))

#select() selects columns, filter() selects rows
#death <- pop_death %>%
#  select(-paste(2011:2021, "Pop", sep = "_")) %>%
#  pivot_longer(
#    cols = contains("Deaths"),
#    names_to = "year", values_to = "death"
#    ) %>%
#  mutate(year = str_replace(year, "_Deaths", ""))

#pop_death <- left_join(pop, death) %>% 
#relocate(year, .before = sex) %>% 
#arrange(year)
 
   
### An alternative pivot longer approach to replace lines 50-68 ----  
pop_death <- pop_death %>%
  pivot_longer(
    cols = `2011_Pop`:`2021_Deaths`,
    names_to = c("year", ".value"),
    names_sep = "_"
  ) %>%
  rename(pop = Pop, death = Deaths) %>%
  relocate(year, .before = sex) %>%    #Move the year column to the left
  arrange(year)      #Arrange in sequential order
  
## Transform 'year' values from character to numeric ----
#mutate(year = as.numeric(year))

## Check that 'year' values have been changed from character to numeric ----
#is.numeric(pop_death$year)


