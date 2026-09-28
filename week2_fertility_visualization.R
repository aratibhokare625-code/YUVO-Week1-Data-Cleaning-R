# YUVO Intern - Week 2
# Data Visualization and Insight Communication using R
# Name: Arati Bhokare
# Dataset: World Bank Fertility Rate (cleaned Week 1 dataset)

library(readr)
library(dplyr)
library(ggplot2)

# Load the cleaned Week 1 dataset
data <- read_csv("week1_fertility_cleaned.csv", show_col_types = FALSE)

# Basic checks
str(data)
summary(data$FertilityRate_Clean)

# 1. Line chart: global mean fertility rate by year
year_mean <- data %>%
  group_by(Year) %>%
  summarise(Mean_Fertility = mean(FertilityRate_Clean, na.rm = TRUE))

ggplot(year_mean, aes(x = Year, y = Mean_Fertility)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Global Mean Fertility Rate, 1960-2011",
    x = "Year",
    y = "Mean fertility rate (births per woman)"
  ) +
  theme_minimal()

# 2. Bar chart: top 10 fertility rates in 2011
top10_2011 <- data %>%
  filter(Year == 2011) %>%
  group_by(`Country Name`) %>%
  summarise(FertilityRate = mean(FertilityRate_Clean, na.rm = TRUE)) %>%
  arrange(desc(FertilityRate)) %>%
  slice_head(n = 10)

ggplot(top10_2011,
       aes(x = reorder(`Country Name`, FertilityRate),
           y = FertilityRate)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Fertility Rates in 2011",
    x = "Country",
    y = "Fertility rate (births per woman)"
  ) +
  theme_minimal()

# 3. Scatter plot: year versus fertility rate
set.seed(42)
sample_data <- data %>% slice_sample(n = min(1800, nrow(data)))

ggplot(sample_data, aes(x = Year, y = FertilityRate_Clean)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Fertility Rate vs Year",
    x = "Year",
    y = "Fertility rate (births per woman)"
  ) +
  theme_minimal()

# 4. Histogram: distribution of cleaned fertility rates
ggplot(data, aes(x = FertilityRate_Clean)) +
  geom_histogram(bins = 30, colour = "black") +
  labs(
    title = "Distribution of Cleaned Fertility Rates",
    x = "Fertility rate (births per woman)",
    y = "Number of country-year observations"
  ) +
  theme_minimal()

# 5. Boxplot at selected years
selected <- data %>% filter(Year %in% c(1960, 1980, 2000, 2011))

ggplot(selected, aes(x = factor(Year), y = FertilityRate_Clean)) +
  geom_boxplot() +
  labs(
    title = "Fertility Rate Distribution at Selected Years",
    x = "Year",
    y = "Fertility rate (births per woman)"
  ) +
  theme_minimal()

# Correlation for descriptive interpretation
cor(data$Year, data$FertilityRate_Clean, use = "complete.obs")
