
# 1. Load Libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, zoo, readr, writexl, janitor)

# 2. Load Data
# Note: skip = 1 removes the metadata title row to read headers correctly
climate_raw <- read_csv("data/climate_data.csv", show_col_types = FALSE, skip = 1) %>% 
  clean_names()

dams_raw <- read_csv("data/dam_data.csv", show_col_types = FALSE, skip = 1) %>% 
  clean_names()

# 3. Data Cleaning: Climate Data
climate_clean <- climate_raw %>%
  filter(province == "Western Cape") %>%
  select(
    Year = year,
    Month = month,
    Temp_Max = maximum_temperature_c,
    Temp_Min = minimum_temperature_c,
    Temp_Avg = average_temperature_c,
    Pressure = air_pressure_h_pa,
    Wind_Speed = wind_speed_km_h,
    Humidity = humidity_percent,
    Precipitation = precipitation_mm
  ) %>%
  mutate(Date = make_date(Year, Month, 1))

# 4. Data Cleaning: Dam Data
dams_clean <- dams_raw %>%
  filter(province == "Western Cape") %>%
  select(
    Year = year,
    Month = month,
    Dam_Level_Pct = dam_level_percent,
    Dam_Name = dam_name
  ) %>%
  mutate(Date = make_date(Year, Month, 1)) %>%
  # Aggregate specific dams to provincial level
  group_by(Date) %>%
  summarise(
    Avg_Dam_Level = mean(Dam_Level_Pct, na.rm = TRUE),
    Max_Dam_Level = max(Dam_Level_Pct, na.rm = TRUE),
    .groups = 'drop'
  )

# 5. Merge Datasets
full_data <- left_join(climate_clean, dams_clean, by = "Date")

# 6. Feature Engineering
# Define 90th percentile threshold for extreme rainfall events
precip_90th <- quantile(full_data$Precipitation, 0.90, na.rm = TRUE)

full_data <- full_data %>%
  mutate(
    Season = case_when(
      Month %in% c(12, 1, 2) ~ "Summer",
      Month %in% c(3, 4, 5) ~ "Autumn",
      Month %in% c(6, 7, 8) ~ "Winter",
      TRUE ~ "Spring"
    ),
    # Target Definitions
    Flood_Severity = Precipitation, # Continuous target
    Flood_Event_Flag = if_else(Precipitation > precip_90th, 1, 0), # Binary target
    
    # Lag Features (1-2 months prior)
    Precip_Lag1 = lag(Precipitation, 1),
    Precip_Lag2 = lag(Precipitation, 2),
    Dam_Lag1 = lag(Avg_Dam_Level, 1)
  ) %>%
  drop_na()

# 7. Save Output
write_csv(full_data, "output/processed_data.csv")
message("Data preparation complete. Output saved to 'output/processed_data.csv'")