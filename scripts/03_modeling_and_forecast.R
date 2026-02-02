# Training predictive models, evaluate performance, and forecast future flood events.
# Loading libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, caret, forecast, randomForest, ggplot2)

#Loading and Splitting Data 
data <- read_csv("output/processed_data.csv", show_col_types = FALSE)

#Ensure Date is formatted correctly for plotting
if ("Date" %in% names(data)) {
  data$Date <- as.Date(data$Date)
}

# Splitting fata into Train (80%) and Test (20%)
split <- floor(0.8 * nrow(data))
train <- data[1:split, ]
test  <- data[(split + 1):nrow(data), ]

# Training Models 
# Model 1: Linear Regression
m_lm  <- lm(Precipitation ~ Temp_Avg + Pressure + Humidity + Dam_Lag1 + Season, data = train)

# Model 2: Random Forest (Machine Learning)
set.seed(123) # Ensure reproducibility
m_rf  <- randomForest(Precipitation ~ Temp_Avg + Pressure + Humidity + Dam_Lag1, data = train, ntree=500)

# Model 3 GLM Gamma (Statistical)
m_glm <- glm(Precipitation ~ Temp_Avg + Pressure + Humidity + Season, family = Gamma(link="log"), data = train)

# Evaluating each models performance 
calc_rmse <- function(act, pred) sqrt(mean((act - pred)^2))

perf <- data.frame(
  Model = c("Linear Regression", "Random Forest", "GLM Gamma"),
  RMSE = c(calc_rmse(test$Precipitation, predict(m_lm, test)),
           calc_rmse(test$Precipitation, predict(m_rf, test)),
           calc_rmse(test$Precipitation, predict(m_glm, test, type="response")))
)

# writing out the performance of the models
write_csv(perf, "output/model_performance_metrics.csv")


# Model Comparison (Actual vs Predicted)
# Create a dataframe with predictions for the Test set
test_results <- test %>%
  select(Date, Precipitation) %>%
  rename(Actual = Precipitation) %>%
  mutate(
    Linear_Reg = predict(m_lm, newdata = test),
    Random_Forest = predict(m_rf, newdata = test),
    GLM_Gamma = predict(m_glm, newdata = test, type = "response")
  ) %>%
  pivot_longer(cols = c("Actual", "Linear_Reg", "Random_Forest", "GLM_Gamma"),
               names_to = "Model", 
               values_to = "Rainfall_mm")

# Plot the comparison
p_comp <- ggplot(test_results, aes(x = Date, y = Rainfall_mm, color = Model, linetype = Model)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Actual" = "black", 
                                "Linear_Reg" = "#D55E00", 
                                "Random_Forest" = "#0072B2", 
                                "GLM_Gamma" = "#009E73")) +
  scale_linetype_manual(values = c("Actual" = "solid", 
                                   "Linear_Reg" = "dashed", 
                                   "Random_Forest" = "dashed", 
                                   "GLM_Gamma" = "dashed")) +
  labs(title = "Model Comparison: Actual vs Predicted Rainfall (Test Set)",
       subtitle = "Evaluating how well each model predicts unseen data (2021)",
       y = "Precipitation (mm)",
       x = "Date") +
  theme_minimal()

ggsave("output/Model_Comparison.png", plot = p_comp, width = 10, height = 6)

# Future Forecast (ARIMA)
# Convert to Time Series object
ts_data <- ts(data$Precipitation, start=c(data$Year[1], data$Month[1]), frequency=12)

# Fit ARIMA model
m_arima <- auto.arima(ts_data, seasonal=TRUE)

# Forecast 5 years (60 months)
fc <- forecast(m_arima, h=60)

# Save Forecast Plot
png("output/Forecast_2026.png", width = 1000, height = 600)
plot(fc, main = "5-Year Precipitation Forecast (2022-2026)", col="darkblue", ylab="Precipitation (mm)")
dev.off()
