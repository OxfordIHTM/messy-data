# Manual attempt to clean occupational_health dataset ----


# Load libraries ----
library(openxlsx2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
#library(tidyverse)


## Read datasets needed ----
Feb2021 <- read_xlsx(file = "data/occupational_health.xlsx", sheet = 2, start_row = 6)
Mar2021 <- read_xlsx(file = "data/occupational_health.xlsx", sheet = 3, start_row = 6)

Feb2021 %>% 
  rename(age = Age) %>%
  rename_with(
    .fn = function(x) sub(pattern = "M_", replacement = "", x = x),
    .cols = `2011_M_Pop`:`2021_M_Deaths`
  )