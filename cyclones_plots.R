# Read and process occupational health data ------------------------------------

## Load libraries ----
library(openxlsx2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

### Alternative code to loop through the sheets to read ----
for (i in 2:6) {
assign(
paste("sheet", i, sep = ""), 
read_xlsx(file = "data/cyclones.xlsx", sheet = i, start_row = 1)
  )
 }

### row bind sheet1 and sheet2 and then pivot longer ----
cyclones <- bind_rows(sheet2, sheet3, sheet4, sheet5, sheet6) 


count(cyclones,category_name)

count(cyclones,category_name, year)

#create barplot 

cyclone_table <- table(cyclones$category_name, cyclones$year )

barplot(cyclone_table, xlab = "category_name", ylab = "year")

count(cyclones,category_name, year) |>
  ggplot(aes(x=year, y=n, fill = category_name)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values=brewer.pal(n=5, name = "Pastel1"))



