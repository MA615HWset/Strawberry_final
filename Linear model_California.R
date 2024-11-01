rm(list = ls())
library(knitr)  
library(kableExtra)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(dplyr)

sur_fer <- read_csv("E:/Desktop/BU/2024 Fall/MA615/Strawberry/sur_fer.csv", col_names = TRUE, show_col_types = FALSE )

sur_total <- read_csv("E:/Desktop/BU/2024 Fall/MA615/Strawberry/sur_total.csv", col_names = TRUE, show_col_types = FALSE )

sur_chem <- read_csv("E:/Desktop/BU/2024 Fall/MA615/Strawberry/sur_chem.csv", col_names = TRUE, show_col_types = FALSE )

# Only California
chemical_california <- subset(sur_chem, State == "CALIFORNIA")

#### fer
sur_fer_california <- subset(sur_fer, State == "CALIFORNIA")

sur_fer_california_filtered <- subset(sur_fer_california, 
                                      measure == "MEASURED IN LB / ACRE / APPLICATION")

sur_fer_california_filtered <- sur_fer_california_filtered[, c(1, 2, 4,7,8, 9, 11,12)]

###### total

sur_total_california <- subset(sur_total, State == "CALIFORNIA")

sur_total_california_filtered <- subset(sur_total_california, 
                                        Period == "YEAR"& 
                                           mkt == "ACRES HARVESTED")

sur_total_california_filtered <- sur_total_california_filtered[, c(1, 2, 4,6,9, 12)]


# head(chemical_california)
# head(sur_fer_california_filtered)
# head(sur_total_california_filtered)
############################
sur_total_california_filtered$Harvest_Level <- cut(as.numeric(sur_total_california_filtered$Value),
                                                   breaks = 3,
                                                   labels = c("Low", "Medium", "High"))

# Summarize fertilizer and chemical data for merging
fert_summary <- sur_fer_california_filtered %>%
  group_by(Year) %>%
  summarise(Total_Fertilizer = sum(as.numeric(Value), na.rm = TRUE), .groups = "drop")

chem_summary <- chemical_california %>%
  group_by(Year) %>%
  summarise(Total_Chemicals = sum(as.numeric(Value), na.rm = TRUE), .groups = "drop")

# Merge all data based on Year
combined_data <- sur_total_california_filtered %>%
  inner_join(fert_summary, by = "Year") %>%
  inner_join(chem_summary, by = "Year")

