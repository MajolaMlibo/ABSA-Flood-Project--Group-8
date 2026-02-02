pacman::p_load(tidyverse, caret, forecast, randomForest, glmnet)

data <- read_csv("output/processed_data.csv", show_col_types = FALSE)


# 1. Train-Test Split (Time-Ordered)
split_idx <- floor(0.8 * nrow(data))
train_data <- data[1:split_idx, ]
test_data  <- data[(split_idx + 1):nrow(data), ]

# 2. Model Training
# Model A: Linear Regression (Baseline)
# Removing Temp_Max/Min due to multicollinearity identified in EDA
m_lm <- lm(Precipitation ~ Temp_Avg + Pressure + Wind_Speed + Humidity + Dam_Lag1 + Season, 
           data = train_data)

# Model B: Random Forest (Non-Linear)
set.seed(2026)
m_rf <- randomForest(Precipitation ~ Temp_Avg + Pressure + Wind_Speed + Humidity + Dam_Lag1, 
                     data = train_data, ntree = 500)

# Model C: GLM Gamma (Handles skewed, positive-only data)
m_glm <- glm(Precipitation ~ Temp_Avg + Pressure + Wind_Speed + Humidity + Season, 
             family = Gamma(link = "log"), 
             data = train_data)

# -------------------------------------------------------------------------
# 3. Model Evaluation
# -------------------------------------------------------------------------
calc_rmse <- function(actual, pred) { sqrt(mean((actual - pred)^2)) }

preds <- data.frame(
  Actual = test_data$Precipitation,
  LM = predict(m_lm, test_data),
  RF = predict(m_rf, test_data),
  GLM = predict(m_glm, test_data, type = "response")
)

performance <- data.frame(
  Model = c("Linear Regression", "Random Forest", "GLM (Gamma)"),
  RMSE  = c(calc_rmse(preds$Actual, preds$LM),
            calc_rmse(preds$Actual, preds$RF),
            calc_rmse(preds$Actual, preds$GLM))
)

write_csv(performance, "output/model_performance_metrics.csv")

# -------------------------------------------------------------------------
# 4. Forecast to 2026 (ARIMA)
# -------------------------------------------------------------------------
# Converting target variable to Time Series object (monthly frequency)
ts_precip <- ts(data$Precipitation, start = c(data$Year[1], data$Month[1]), frequency = 12)

# Fit Auto-ARIMA
m_arima <- auto.arima(ts_precip, seasonal = TRUE)

# Forecast 5 Years (60 Months)
fc_2026 <- forecast(m_arima, h = 60)

# Save Forecast Plot
png("output/Forecast_2026.png", width = 1000, height = 600)
plot(fc_2026, main = "Flood Severity Forecast (2022-2026)", 
     ylab = "Projected Precipitation (mm)", xlab = "Year", col="darkblue")
dev.off()

message("Modeling complete. Forecast generated.")