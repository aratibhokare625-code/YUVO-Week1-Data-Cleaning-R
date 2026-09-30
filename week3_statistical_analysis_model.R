# YUVO Week 3 - Statistical Analysis and Predictive Modeling using R
# Name: Arati Bhokare
# Dataset: World Bank Fertility Rate (1960-2011), cleaned in Week 1

library(readr)
library(dplyr)
library(ggplot2)
library(caret)

data <- read_csv("week1_fertility_cleaned.csv")

analysis_df <- data %>%
  select(Country_Name = `Country Name`,
         Country_Code = `Country Code`,
         Year,
         FertilityRate_Clean) %>%
  filter(!is.na(FertilityRate_Clean))

# Descriptive statistics
summary(analysis_df$FertilityRate_Clean)
mean(analysis_df$FertilityRate_Clean)
sd(analysis_df$FertilityRate_Clean)

# Correlation and hypothesis test
cor_test <- cor.test(analysis_df$Year, analysis_df$FertilityRate_Clean,
                     method = "pearson")
print(cor_test)

# Normality check (Shapiro-Wilk is applied to a sample because n is large)
set.seed(42)

# Train/test split: 80/20
train_index <- createDataPartition(analysis_df$FertilityRate_Clean,
                                   p = 0.80, list = FALSE)

train_data <- analysis_df[train_index, ]
test_data  <- analysis_df[-train_index, ]

# Linear regression
model <- lm(FertilityRate_Clean ~ Year, data = train_data)
summary(model)

# Predictions and evaluation
test_data$Predicted <- predict(model, newdata = test_data)
rmse <- sqrt(mean((test_data$FertilityRate_Clean - test_data$Predicted)^2))
mae <- mean(abs(test_data$FertilityRate_Clean - test_data$Predicted))
r2 <- cor(test_data$FertilityRate_Clean, test_data$Predicted)^2

cat("Test RMSE:", rmse, "\n")
cat("Test MAE:", mae, "\n")
cat("Test R-squared:", r2, "\n")

# 5-fold cross-validation
ctrl <- trainControl(method = "cv", number = 5)
cv_model <- train(FertilityRate_Clean ~ Year,
                  data = analysis_df,
                  method = "lm",
                  trControl = ctrl,
                  metric = "RMSE")
print(cv_model)

# Diagnostic plots
par(mfrow = c(2,2))
plot(model)

# Residual normality check
res <- residuals(model)
set.seed(42)
res_sample <- sample(res, min(length(res), 5000))
shapiro.test(res_sample)

# Optional polynomial comparison
quad_model <- lm(FertilityRate_Clean ~ Year + I(Year^2), data = train_data)
summary(quad_model)

# Visualizations
ggplot(analysis_df, aes(x = Year, y = FertilityRate_Clean)) +
  geom_point(alpha = 0.15) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Fertility Rate vs Year",
       x = "Year", y = "Cleaned Fertility Rate") +
  theme_minimal()

ggplot(test_data, aes(x = Predicted, y = FertilityRate_Clean)) +
  geom_point(alpha = 0.25) +
  geom_abline(slope = 1, intercept = 0) +
  labs(title = "Observed vs Predicted Fertility Rate",
       x = "Predicted", y = "Observed") +
  theme_minimal()
