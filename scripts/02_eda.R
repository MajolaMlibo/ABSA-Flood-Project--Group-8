# Purpose: Perform Exploratory Data Analysis (EDA) and save visualizations to the output folder.

# Load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, ggplot2, corrplot, gridExtra)

data_path <- "output/processed_data.csv" 
output_dir <- "output"

# Create the output directory if it does not exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  message("Created output directory: ", output_dir)
}

# Loading Data
if (file.exists(data_path)) {
  df <- read.csv(data_path)
  message("Data loaded successfully.")
} else {
  # Fallback: If running from the 'scripts' folder, try the parent directory
  if (file.exists(paste0("../", data_path))) {
     data_path <- paste0("../", data_path)
     output_dir <- paste0("../", output_dir)
     df <- read.csv(data_path)
     message("Data loaded successfully (adjusted for scripts directory).")
  } else {
     stop("Error: processed_data.csv not found. Please ensure your working directory is the project root.")
  }
}

# Ensure Date column is correctly formatted
if ("Date" %in% names(df)) {
  df$Date <- as.Date(df$Date)
}

numeric_vars <- df %>% select_if(is.numeric)

if (ncol(numeric_vars) > 1) {
  png(filename = file.path(output_dir, "CorrHeatMap.png"), width = 800, height = 800)
  
  # Calculate correlation matrix
  corr_matrix <- cor(numeric_vars, use = "complete.obs")
  
  # Plot heatmap
  corrplot(corr_matrix, 
           method = "color", 
           type = "upper", 
           tl.col = "black", 
           tl.srt = 45, 
           addCoef.col = "black", # Add numbers
           title = "Correlation Matrix", 
           mar = c(0,0,1,0))
  
  dev.off()
}

# Time Series Plot
# Plotting Precipitation over time to spot any historical trends.
message("Generating Time Series Plot...")
ts_y_var <- "Precipitation" 

if (ts_y_var %in% names(df)) {
  p_ts <- ggplot(df, aes(x = Date, y = .data[[ts_y_var]])) +
    geom_line(color = "#0072B2", size = 1) +
    labs(title = paste("Time Series Analysis of", ts_y_var),
         subtitle = "Historical rainfall trends over time",
         x = "Date",
         y = ts_y_var) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14))
  
  ggsave(filename = file.path(output_dir, "TimeSeries.png"), plot = p_ts, width = 10, height = 6)
}

# Box Plot
# Checking the distribution of Average Temperature by month.
box_y_var <- "Temp_Avg"

if ("Date" %in% names(df) & box_y_var %in% names(df)) {
  # Extract Month
  df$Month_Name <- month(df$Date, label = TRUE, abbr = TRUE)
  
  p_box <- ggplot(df, aes(x = Month_Name, y = .data[[box_y_var]], fill = Month_Name)) +
    geom_boxplot(alpha = 0.7) +
    labs(title = paste("Monthly Distribution of", box_y_var),
         x = "Month",
         y = box_y_var) +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold"))
  
  ggsave(filename = file.path(output_dir, "BoxPlot.png"), plot = p_box, width = 10, height = 6)
}

# Scatter Plot
# Investigating the relationship between Average Temperature and Precipitation.
message("Generating Scatter Plot...")
scatter_x_var <- "Temp_Avg"     # Predictor
scatter_y_var <- "Precipitation" # Response

if (scatter_x_var %in% names(df) & scatter_y_var %in% names(df)) {
  p_scatter <- ggplot(df, aes(x = .data[[scatter_x_var]], y = .data[[scatter_y_var]])) +
    geom_point(alpha = 0.6, color = "#D55E00") +
    geom_smooth(method = "lm", color = "black", se = FALSE) + # Add trend line
    labs(title = paste("Relationship:", scatter_x_var, "vs", scatter_y_var),
         subtitle = paste("Correlation analysis between", scatter_x_var, "and", scatter_y_var),
         x = scatter_x_var,
         y = scatter_y_var) +
    theme_minimal()
  
  ggsave(filename = file.path(output_dir, "scatterPlot.png"), plot = p_scatter, width = 8, height = 6)
}