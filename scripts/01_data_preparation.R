# Purpose: Load, clean, merge, and engineer features for Flood Risk Analysis

# 1. Load Libraries
#if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, zoo, readr, writexl)

# 2. Load Data
# Added 'skip = 1' because the first row of the CSVs contains a title/metadata, not headers.
# MODIFICATION: Using read_delim with delim = ";" to correctly parse the semicolon-separated files.
climate_raw <- read_delim("data/climate_data.csv", delim = ";", show_col_types = FALSE, skip = 1)
dams_raw <- read_delim("data/dam_data.csv", delim = ";", show_col_types = FALSE, skip = 1)

summary(climate_raw )
summary(dams_raw )

# 3. Data Cleaning: Climate Data
# Select relevant columns and fix types
# MODIFICATION: Refactored pipeline to filter first, then select columns by Index (position) 
# instead of Name. This avoids errors caused by special characters (e.g., °) or encoding mismatches.
# Index Mapping: 1=Province, 2=Year, 3=Month, 4=Max, 5=Min, 6=Avg, 7=Pressure, 8=Wind, 9=Humidity, 10=Precip
climate_clean <- climate_raw %>%
  filter(Province == "Western Cape") %>%
  select(
    Year = 2,
    Month = 3,
    Temp_Max = 4,
    Temp_Min = 5,
    Temp_Avg = 6,
    Pressure = 7,
    Wind_Speed = 8,
    Humidity = 9,
    Precipitation = 10
  ) %>%
  mutate(Date = make_date(Year, Month, 1)) %>%
  select(Date, Year, Month, Temp_Max, Temp_Min, Temp_Avg, Pressure, Wind_Speed, Humidity, Precipitation)

# 4. Data Cleaning: Dam Data
# Dams are specific locations, but climate is provincial. 
# We must aggregate dam levels to get a "Western Cape Water Stress" index.
# MODIFICATION: Same index-based selection strategy for robustness.
# Index Mapping: 1=Province, 2=Year, 3=Month, 6=Dam Level (%), 7=Dam Name
dams_clean <- dams_raw %>%
  filter(Province == "Western Cape") %>%
  select(
    Year = 2,
    Month = 3,
    Dam_Level_Pct = 6,
    Dam_Name = 7
  ) %>%
  mutate(Date = make_date(Year, Month, 1)) %>%
  # Aggregation strategy: Average dam level across the province to match climate granularity
  group_by(Date) %>%
  summarise(
    Avg_Dam_Level = mean(Dam_Level_Pct, na.rm = TRUE),
    Max_Dam_Level = max(Dam_Level_Pct, na.rm = TRUE), # Identifying if ANY dam is overflowing
    .groups = 'drop'
  )

# 5. Merge Datasets
full_data <- left_join(climate_clean, dams_clean, by = "Date")

# 6. Feature Engineering & Target Creation
# JUSTIFICATION: The case study asks to predict "Flood Events" but does not provide a labeled column.
# We define a "Flood Event" based on hydrological risk factors:
# 1. High Precipitation (Severity proxy)
# 2. Saturated Ground/Dams (Risk multiplier)

# Define Thresholds (derived from statistical distribution of the data)
precip_90th <- quantile(full_data$Precipitation, 0.90, na.rm = TRUE)

full_data <- full_data %>%
  mutate(
    # Feature: Seasonality (Floods often seasonal)
    Season = case_when(
      Month %in% c(12, 1, 2) ~ "Summer",
      Month %in% c(3, 4, 5) ~ "Autumn",
      Month %in% c(6, 7, 8) ~ "Winter",
      TRUE ~ "Spring"
    ),
    # Target 1: Flood_Severity (Continuous) -> Modeled by Precipitation amount
    Flood_Severity = Precipitation,
    
    # Target 2: Flood_Frequency_Flag (Binary) -> Did a high-risk event occur?
    # Logic: If rain is in top 10% AND dams are relatively full (>70%), risk is critical.
    Flood_Event_Flag = if_else(Precipitation > precip_90th, 1, 0),
    
    # Lagged Variables (Predictors): Weather trends *precede* floods
    Precip_Lag1 = lag(Precipitation, 1),
    Precip_Lag2 = lag(Precipitation, 2),
    Dam_Lag1 = lag(Avg_Dam_Level, 1)
  ) %>%
  drop_na() # Remove first 2 rows due to lags

# 7. Save Processed Data
write_csv(full_data, "output/processed_data.csv")
message("Data preparation complete. File saved to output/processed_data.csv")