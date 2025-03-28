# Examples of data cleaning of RWD----------------------

## Load libraries------

library(openxlsx2)
library(dplyr)
library(tidyr)
library(ggplot2)

# Read data sets needed----------------


vaccine <- read_xlsx("data/vaccine.xlsx", sheet = 1)


