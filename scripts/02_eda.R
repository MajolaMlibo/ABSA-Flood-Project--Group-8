pacman::p_load(tidyverse, ggplot2, corrplot, gridExtra, scales)

data <- read_csv("output/processed_data.csv", show_col_types = FALSE)

# 1. Time Series Analysis (Matches 'TimeSeries.png')
# Visualizing the interaction between rainfall spikes and dam saturation
p_ts <- ggplot(data, aes(x = Date)) +
  geom_line(aes(y = Precipitation, color = "Precipitation (mm)"), linewidth = 1) +
  geom_line(aes(y = Avg_Dam_Level, color = "Avg Dam Level (%)"), linewidth = 1) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Dam Level (%)")) +
  scale_color_manual(values = c("Precipitation (mm)" = "#1f77b4", "Avg Dam Level (%)" = "#d62728")) +
  labs(title = "Historical Trends: Precipitation vs Dam Levels (2017-2021)",
       x = "Date", y = "Precipitation (mm)", color = "Metric") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("output/TimeSeries.png", plot = p_ts, width = 10, height = 6)

# 2. Correlation Heatmap (Matches 'CorrHeatMap.png')
# Checking for multicollinearity among predictors
num_vars <- data %>% 
  select(Temp_Max, Temp_Min, Pressure, Wind_Speed, Humidity, Precipitation, Avg_Dam_Level)

png("output/CorrHeatMap.png", width = 800, height = 800)
corrplot(cor(num_vars), method = "color", type = "upper", order = "hclust", 
         addCoef.col = "black", tl.col = "black", 
         title = "Feature Correlation Matrix", mar=c(0,0,2,0))
dev.off()

# 3. Box Plot Analysis (Matches 'BoxPlot.png')
# Seasonal distribution of rainfall
p_box <- ggplot(data, aes(x = Season, y = Precipitation, fill = Season)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Seasonal Rainfall Distribution",
       subtitle = "Winter shows significantly higher variance and median rainfall",
       x = "Season", y = "Precipitation (mm)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("output/BoxPlot.png", plot = p_box, width = 8, height = 6)

# 4. Scatter Plot (Matches 'scatterPlot.png')
# Relationship between Humidity and Rainfall (Linearity check)
p_scat <- ggplot(data, aes(x = Humidity, y = Precipitation)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Correlation: Humidity vs Precipitation",
       x = "Humidity (%)", y = "Precipitation (mm)") +
  theme_minimal()

ggsave("output/scatterPlot.png", plot = p_scat, width = 8, height = 6)

message("EDA Complete. 4 Images saved to output/")