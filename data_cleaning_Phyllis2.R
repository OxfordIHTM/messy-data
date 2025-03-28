# Tidying cyclones data set

## Load libraries ----
library(openxlsx2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

for (i in 2:6) {
    assign(
      paste("sheet", i, sep = ""),
      read_xlsx(file = "data/cyclones.xlsx", sheet = i, start_row = 1)
    )
  }

### Row bind all sheets and then pivot longer ----
cyclones <- bind_rows(sheet2, sheet3, sheet4, sheet5, sheet6)

## Summarising cyclones output

table(cyclones$category_name)
table(cyclones$category_name, cyclones$year)

count(cyclones, category_name)
count(cyclones, category_name, year)

cyclones |>
  count(category_name, year) |>
  ggplot(aes(x = year, y = n, fill = category_name)) +
  geom_bar(position = "stack", stat = "identity") + 
  scale_fill_manual(values = brewer.pal(n=5, name = "Pastel1")) +
  theme_minimal()


