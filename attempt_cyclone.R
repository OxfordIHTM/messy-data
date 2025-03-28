# Read and process cyclones data -----------------------------------------------

## Load libraries ----
library(readxl)
#library(openxlsx2)
library(dplyr)
library(tidyr)
library(ggplot2)

#Read cyclones data------

#2017 <- read_xlsx(file = "data/cyclones.xlsx", sheet = 2 )
#2018 <- read_xlsx(file = "data/cyclones.xlsx", sheet = 3 )
#2019 <- read_xlsx(file = "data/cyclones.xlsx", sheet = 4 )
#2020 <- read_xlsx(file = "data/cyclones.xlsx", sheet = 5 )
#2021 <- read_xlsx(file = "data/cyclones.xlsx", sheet = 6 )

#for (i in 2:6) {
 # assign(
  #  paste("sheet", i, sep = ""), 
   # read_xlsx(file = "data/cyclones.xlsx", sheet = i)
  #)
#}

cyclones_attempt <- Map(
  f = read_xlsx,
  path = "data/cyclones.xlsx",
  sheet = 2:6,
    col_names = TRUE
) |>
    bind_rows()

typhoons_summary <- table(cyclones$category_code)

typhoons_by_year <- table((cyclones$year) , (cyclones$category_code))

#gt_summary_package for thesis

barplot(typhoons_by_year)

cyclones_attempt |> 
  count(category_code,year) |>
  ggplot(aes(x= year, y= n, fill = category_code) ) +
             geom_violin(colour = "pink", fill = "blue", alpha = 0.3)+
  geom_jitter(colour = "red", size = 3, width = 0.2)



