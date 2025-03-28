# Examples of data cleaning of RWD: cyclones dataset ----


# Load libraries ----
library(openxlsx2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
#library(tidyverse)


## Read datasets needed ----
### For loop approach
for (i in 2:6) {
  assign(
    paste0("sheet", i),
    read_xlsx(file = "data/cyclones.xlsx", sheet = i, start_row = 1))
}

### Manual approach ----
# sheet2 <- read_xlsx(file = "data/cyclones.xlsx", sheet = 2, start_row = 1)
# sheet3 <- read_xlsx(file = "data/cyclones.xlsx", sheet = 3, start_row = 1)
# sheet4 <- read_xlsx(file = "data/cyclones.xlsx", sheet = 4, start_row = 1)
# sheet5 <- read_xlsx(file = "data/cyclones.xlsx", sheet = 5, start_row = 1)
# sheet6 <- read_xlsx(file = "data/cyclones.xlsx", sheet = 6, start_row = 1)

### Map approach ----
# rows <- list(2:23, 2:22, 2:22, 2:23, 2:16)
# 
# cyclones <- Map(
#   f = read_xlsx,
#   file = "data/cyclones.xlsx",
#   sheet = 2:6,
#   rows = rows,
#   col_names = FALSE
# )%>%
#   lapply(function(cyclones) {
#     cyclones %>% mutate_all(as.character)  # Convert all columns to character
#   }) %>%
#   bind_rows(.id = "id")
#   rename(year = A, 
#   category_code = B, 
#   category_name = C, 
#   name = D, 
#   rmsc_name = E, 
#   start = F, 
#   end = G, 
#   pressure = H, 
#   speed = I)


## Row bind ----
#cyclones <- bind_rows(sheet2, sheet3, sheet4, sheet5, sheet6, .id = "id") 
cyclones <- bind_rows(mget(paste0("sheet", 2:6)), .id = "id")

## Create summary table of # of typhoons per category ----
summary_table <- cyclones %>%
  count(category_name)

## Create summary table of # of typhoons per category by year ----
summary_table_yr <- cyclones %>%
  count(year, category_name) %>%
pivot_wider(names_from = `year`, values_from = `n`) #To alter presentation of data

## Create bar plot of # of typhoons per category ----
ggplot(summary_table, aes(x = factor(category_name, 
  levels = c("Tropical Depression", "Tropical Storm", "Severe Tropical Storm", "Typhoon", "Super Typhoon")), 
  y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +  # Use stat = "identity" to use pre-counted values
  labs(title = "Number of Typhoons by Category",
       x = "Typhoon Category",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 12), axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7))

#Create a plot of the mean duration in hours per category of typhoon ----
mean_durations <- cyclones %>%
  mutate(
    duration = as.numeric(difftime(end, start, units = "hours")),  # Calculate duration in hours
    ) %>%
  group_by(category_name) %>%
  summarise(mean_duration = mean(duration)) 

sum_table_means <- summary_table %>%
  left_join(mean_durations, by = "category_name")

ggplot(sum_table_means, aes(x = factor(category_name, 
                                     levels = c("Tropical Depression", "Tropical Storm", "Severe Tropical Storm", "Typhoon", "Super Typhoon")), 
                          y = mean_duration)) +
  geom_bar(stat = "identity", fill = "pink") +  # Use stat = "identity" to use pre-counted values
  labs(title = "Mean Duration of Typhoons by Category",
       x = "Typhoon Category",
       y = "Mean Duration") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 12), axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7))

