
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

data <- data %>%
  mutate(across(
    all_of(age_cols),
    ~ as.numeric(ifelse(. %in% c("NULL", "", "NA"), NA, .))
  ))

str(data)


totals_by_sex <- data %>%
  group_by(Sex) %>%
  summarise(
    young_18_30   = sum(Victims_Upto_18_30_Yrs, na.rm = TRUE),
    total_victims = sum(Victims_Total,          na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    other_ages     = total_victims - young_18_30,
    prop_young     = young_18_30 / total_victims,
    prop_young_pct = 100 * prop_young
  )

totals_by_sex  

cont_table <- as.matrix(totals_by_sex[, c("young_18_30", "other_ages")])
rownames(cont_table) <- totals_by_sex$Sex
colnames(cont_table) <- c("Age_18_30", "Other_ages")

cont_table

chisq_result <- chisq.test(cont_table)
chisq_result    # report X-squared, df, and p-value

ggplot(totals_by_sex, aes(x = Sex, y = prop_young_pct)) +
  geom_col() +
  labs(
    title = "Proportion of murder victims aged 18–30 by sex",
    x     = "Sex of victim",
    y     = "Percentage of victims aged 18–30 (%)"
  ) +
  ylim(0, 100) +
  theme_minimal(base_size = 12)

state_props <- data %>%
  group_by(Area_Name, Year, Sex) %>%
  summarise(
    young_18_30   = sum(Victims_Upto_18_30_Yrs, na.rm = TRUE),
    total_victims = sum(Victims_Total,          na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    prop_young = young_18_30 / ifelse(total_victims == 0, NA, total_victims)
  ) %>%
  filter(!is.na(prop_young))

ggplot(state_props, aes(x = prop_young)) +
  geom_histogram(bins = 20) +
  labs(
    title = "Distribution of state–year proportion of 18–30 victims by sex",
    x     = "Proportion of victims aged 18–30",
    y     = "Number of state–year observations"
  ) +
  facet_wrap(~ Sex) +
  theme_minimal(base_size = 12)

