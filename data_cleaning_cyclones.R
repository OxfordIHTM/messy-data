# Read and process cyclones data -----------------------------------------------

## Load libraries ----
library(readxl)     ## note here that I use readxl package rather than openxlsx2
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)


## Read cyclones dataset ----

### Get sheet names for each year dataset only ----
sheet_names <- excel_sheets("data/cyclones.xlsx") |>
  (\(x) x[2:length(x)])()

### Read data from each year sheet and concatenate ----
cyclones <- Map(
  f = read_xlsx,
  path = "data/cyclones.xlsx",
  sheet = sheet_names
) |>
  bind_rows()

### Set category code and category name into factors and date-time variables ----
cyclones <- cyclones |>
  mutate(
    category_code = factor(
      category_code, levels = c("TD", "TS", "STS", "TY", "ST")
    ),
    category_name = factor(
      category_name, 
      levels = c(
        "Tropical Depression", "Tropical Storm", "Severe Tropical Storm",
        "Typhoon", "Super Typhoon"
      )
    )
  )


## Create cyclones summaries and visualisation ----

### Number of cylones per category by year ----
cyclones |>
  count(year, category_name) |>
  pivot_wider(names_from = year, values_from = n)

### Get cyclones category mean pressure and speed ----
cyclones |>
  group_by(category_name) |>
  summarise(
    n = n(),
    mean_pressure = mean(pressure), 
    mean_speed = mean(speed)
  )

### Get cyclone category mean duration (in hours) ----
cyclones |>
  mutate(duration = end - start) |>
  group_by(category_name) |>
  summarise(mean_duration = mean(duration))


### Create cyclones colours vector ----
cyclone_colours <- c(
  "#e5be72", "#4d4d4d", "#5ada3d", "#5630d3", "#e51707",
  "#9c5e60", "#465b92", "#4b876e", "#152127", "#5d0505"
)

### Plot cyclone category mean duration (in hours) ----
cyclones |>
  mutate(duration = end - start) |>
  group_by(category_name) |>
  summarise(mean_duration = as.numeric(mean(duration))) |>
  ggplot(mapping = aes(x = mean_duration, y = category_name)) +
  geom_col(colour = "#4b876e", fill = "#4b876e", alpha = 0.5) +
  labs(
    title = "Mean duration of cyclones",
    subtitle = "By cyclone categories",
    x = "mean duration (hours)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

### Plot cyclone speed by presssure ----
cyclones |>
  dplyr::mutate(year = factor(year)) |>
  ggplot(mapping = aes(x = speed, y = pressure)) +
  geom_point(mapping = aes(colour = category_name), size = 3, alpha = 0.5) +
  scale_colour_manual(
    name = NULL,
    values = c("#9c5e60", "#4b876e", "#465b92", "#e5be72", "#5d0505")
  ) +
  labs(
    title = "Cyclone maximum sustained wind speed and maximum central pressure",
    subtitle = "By cyclone categories and year",
    x = "wind speed (km/h)",
    y = "central pressure (hPa)"
  ) +
  facet_wrap(. ~ year, ncol = 5) +
  theme_bw() +
  theme(
    legend.position = "top",
    strip.background = element_rect(
      fill = alpha("#465b92", 0.7), colour = "#465b92"
    ),
    panel.border = element_rect(colour = "#465b92"),
    panel.grid.minor = element_blank()
  )

### Plot number of cyclones per month by year ----
cyclones |>
  mutate(month = month(start, label = TRUE)) |>
  group_by(month, year) |>
  count() |>
  ungroup() |>
  complete(month, year, fill = list(n = 0)) |>
  arrange(year, month) |>
  ggplot(mapping = aes(x = month, y = n)) +
  geom_col(colour = "#4b876e", fill = "#4b876e", alpha = 0.5) +
  scale_y_continuous(breaks = seq(from = 0, to = 6, by = 1)) +
  labs(
    title = "Number of cyclones over time",
    subtitle = "2017-2021",
    x = NULL,
    y = "n"
  ) +
  facet_wrap(. ~ year, ncol = 5) +
  theme_bw() +
  theme(
    strip.background = element_rect(
      fill = alpha("#465b92", 0.7), colour = "#465b92"
    ),
    panel.border = element_rect(colour = "#465b92"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 10, angle = 90, hjust = 1, vjust = 0.5)
  )

### Boxplot of cyclone speed per year ----
cyclones |>
  mutate(year = factor(year)) |>
  ggplot(mapping = aes(x = year, y = speed)) +
  geom_boxplot(colour = "#4b876e", fill = "#4b876e", alpha = 0.5) +
  labs(
    title = "Distribution of tropical cyclone maximum sustained wind speed",
    subtitle = "2017-2021",
    x = NULL, y = "speed (km/h)"
  ) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank())

### Boxplot with jitter of cyclone speed per year ----
cyclones |>
  mutate(year = factor(year)) |>
  ggplot(mapping = aes(x = year, y = speed)) +
  geom_boxplot(colour = "#4b876e") +
  geom_jitter(
    colour = "#4b876e", fill = "#4b876e", alpha = 0.5,
    shape = 21, size = 2, width = 0.2
  ) +
  labs(
    title = "Distribution of tropical cyclone maximum sustained wind speed",
    subtitle = "2017-2021",
    x = NULL, y = "speed (km/h)"
  ) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank())

### Violin plot with jitter of cyclone speed per year ----
cyclones |>
  mutate(year = factor(year)) |>
  ggplot(mapping = aes(x = year, y = speed)) +
  geom_violin(colour = "#4b876e", fill = "#4b876e", alpha = 0.3) +
  geom_jitter(colour = "#4b876e", size = 3, width = 0.2) +
  labs(
    title = "Distribution of tropical cyclone maximum sustained wind speed",
    subtitle = "2017-2021",
    x = NULL, y = "speed (km/h)"
  ) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank())
