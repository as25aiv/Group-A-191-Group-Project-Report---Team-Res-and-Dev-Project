
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

data <- read_csv("32_Murder_victim_age_sex.csv")

head(data)
str(data)

data <- data %>%
  mutate(
    Sex = case_when(
      grepl("Female", Group_Name, ignore.case = TRUE) ~ "Female",
      grepl("Male",   Group_Name, ignore.case = TRUE) ~ "Male",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Sex))   # keep only Male/Female rows

age_cols <- c(
  "Victims_Above_50_Yrs",
  "Victims_Upto_10_15_Yrs",
  "Victims_Upto_10_Yrs",
  "Victims_Upto_15_18_Yrs",
  "Victims_Upto_18_30_Yrs",
  "Victims_Upto_30_50_Yrs"
)

