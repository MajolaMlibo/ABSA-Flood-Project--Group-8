# -------------------------------------------------------------------------
# Script: 02_eda.R
# Purpose: Visualise patterns, check assumptions, and generate report plots
# -------------------------------------------------------------------------

pacman::p_load(tidyverse, ggplot2, corrplot, gridExtra)

data <- read_csv("output/processed_data.csv", show_col_types = FALSE)

# 1. Time Series Analysis (Trend Identification)
p1 <- ggplot(data, aes(x = Date)) +
  geom_line(aes(y = Precipitation, color = "Precipitation"), linewidth = 1) +
  geom_line(aes(y = Avg_Dam_Level, color = "Dam Level (%)"), linewidth = 1) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Dam Level (%)")) +
  labs(title = "Western Cape: Precipitation vs Dam Levels (2017-2021)",
       subtitle = "High dam levels followed by rain spikes indicate flood risk",
       y = "Precipitation (mm)", x = "Date") +
  theme_minimal() +
  scale_color_manual(values = c("Precipitation" = "blue", "Dam Level (%)" = "red"))

ggsave("output/EDA_TimeSeries.png", plot = p1, width = 10, height = 6)

# 2. Correlation Analysis (Multicollinearity Check)
# Select numeric predictors
num_vars <- data %>% select(Temp_Max, Temp_Min, Pressure, Wind_Speed, Humidity, Precipitation, Avg_Dam_Level)
cor_matrix <- cor(num_vars)

png("output/EDA_CorrelationMatrix.png", width = 800, height = 800)
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", 
         addCoef.col = "black", tl.col = "black", title = "Variable Correlation Matrix", mar=c(0,0,1,0))
dev.off()

# Insight Calculation for Report:
# High correlation expected between Temp_Max and Temp_Min (Multicollinearity risk).
# Strategy: Use PCA or select only one Temp variable, or use regularization (Elastic Net).

# 3. Distribution of Target (Assumption Checking)
p2 <- ggplot(data, aes(x = Precipitation)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Monthly Precipitation", x = "mm", y = "Count") +
  theme_minimal()

ggsave("output/EDA_Distribution.png", plot = p2, width = 6, height = 4)
# Insight: Data is right-skewed. Gamma regression is appropriate for severity modeling.

message("EDA complete. Plots saved to output/")