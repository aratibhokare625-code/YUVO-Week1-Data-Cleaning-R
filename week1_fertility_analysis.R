# YUVO Intern - Week 1: Data Cleaning and Preliminary Analysis with R
# Dataset: World Bank Fertility Rate (1960-2013), distributed via statsmodels
# The workflow uses 1960-2011 so the fully-missing 2012-2013 columns in this
# historical copy do not dominate the analysis.

# 1. Packages
packages <- c("dplyr", "tidyr", "ggplot2", "readr")
new <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new)) install.packages(new)
lapply(packages, library, character.only = TRUE)

# 2. Load data
url <- "https://raw.githubusercontent.com/statsmodels/statsmodels/main/statsmodels/datasets/fertility/fertility.csv"
raw <- read.csv(url, check.names = FALSE, stringsAsFactors = FALSE)

# Keep metadata + 1960-2011
years <- as.character(1960:2011)
raw <- raw[, c("Country Name", "Country Code", years)]

# 3. Inspect structure and summary
str(raw)
summary(raw)

# 4. Reshape from wide to long format
data_long <- raw %>%
  pivot_longer(
    cols = all_of(years),
    names_to = "Year",
    values_to = "FertilityRate"
  ) %>%
  mutate(Year = as.integer(Year)) %>%
  arrange(`Country Name`, Year)

# 5. Missing-value analysis
missing_before <- sum(is.na(data_long$FertilityRate))
print(missing_before)

missing_by_year <- data_long %>%
  group_by(Year) %>%
  summarise(Missing = sum(is.na(FertilityRate)), .groups = "drop")
print(missing_by_year)

# 6. Missing-value handling:
# Linear interpolation within each country. Countries with no observed
# fertility values in 1960-2011 are removed from the analytical dataset.
data_clean <- data_long %>%
  group_by(`Country Name`) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(
    FertilityRate_Clean = if (sum(!is.na(FertilityRate)) >= 1) {
      approx(
        x = Year[!is.na(FertilityRate)],
        y = FertilityRate[!is.na(FertilityRate)],
        xout = Year,
        method = "linear",
        rule = 2
      )$y
    } else {
      rep(NA_real_, length(Year))
    }
  ) %>%
  ungroup()

data_clean <- data_clean %>%
  group_by(`Country Name`) %>%
  filter(!all(is.na(FertilityRate_Clean))) %>%
  ungroup()

# 7. Outlier detection using IQR
Q1 <- quantile(data_clean$FertilityRate_Clean, 0.25, na.rm = TRUE)
Q3 <- quantile(data_clean$FertilityRate_Clean, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

data_clean <- data_clean %>%
  mutate(
    Outlier = FertilityRate_Clean < lower_bound |
              FertilityRate_Clean > upper_bound
  )

print(table(data_clean$Outlier))

# 8. Normalization
# Min-max normalization
min_rate <- min(data_clean$FertilityRate_Clean, na.rm = TRUE)
max_rate <- max(data_clean$FertilityRate_Clean, na.rm = TRUE)

data_clean <- data_clean %>%
  mutate(
    FertilityRate_MinMax =
      (FertilityRate_Clean - min_rate) / (max_rate - min_rate),
    FertilityRate_Z =
      as.numeric(scale(FertilityRate_Clean))
  )

# 9. Encoding categorical variables
# Convert Country Name to factor and demonstrate one-hot encoding.
data_clean$Country_Factor <- as.factor(data_clean$`Country Name`)
country_dummy_example <- model.matrix(~ Country_Factor - 1,
                                      data = data_clean %>% slice_head(n = 20))
print(country_dummy_example[1:5, 1:5])

# 10. Descriptive statistics
print(summary(data_clean$FertilityRate_Clean))
print(
  data_clean %>%
    group_by(Year) %>%
    summarise(
      Mean = mean(FertilityRate_Clean, na.rm = TRUE),
      Median = median(FertilityRate_Clean, na.rm = TRUE),
      SD = sd(FertilityRate_Clean, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    head()
)

# 11. Correlation between year and fertility rate
print(cor(data_clean$Year, data_clean$FertilityRate_Clean,
          use = "complete.obs"))

# 12. Visualizations
ggplot(data_clean, aes(x = Year, y = FertilityRate_Clean)) +
  stat_summary(fun = mean, geom = "line") +
  labs(
    title = "Average Fertility Rate by Year",
    x = "Year", y = "Births per woman"
  ) +
  theme_minimal()

ggplot(data_clean, aes(x = FertilityRate_Clean)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of Fertility Rate",
    x = "Births per woman", y = "Count"
  ) +
  theme_minimal()

selected_years <- data_clean %>%
  filter(Year %in% c(1960, 1980, 2000, 2011))

ggplot(selected_years, aes(x = factor(Year), y = FertilityRate_Clean)) +
  geom_boxplot() +
  labs(
    title = "Fertility Rate Distribution at Selected Years",
    x = "Year", y = "Births per woman"
  ) +
  theme_minimal()

# 13. Save cleaned data
write.csv(data_clean, "week1_fertility_cleaned_R.csv", row.names = FALSE)
