if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, zoo, readr, janitor)

#Loading Data
climate_raw <- read_csv2("data/climate_data.csv", show_col_types = FALSE, skip = 1) %>% 
  clean_names()

dams_raw <- read_csv2("data/dam_data.csv", show_col_types = FALSE, skip = 1) %>% 
  clean_names()

# Cleaning Climate Data
climate_clean <- climate_raw %>%
  filter(province == "Western Cape") %>%
  # Ensure all metric columns are numeric
  mutate(across(c(maximum_temperature_c, minimum_temperature_c, average_temperature_c, 
                  air_pressure_h_pa, wind_speed_km_h, humidity_percent, precipitation_mm), 
                ~as.numeric(as.character(.)))) %>%
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

# Cleaning Dam Data
dams_clean <- dams_raw %>%
  filter(province == "Western Cape") %>%
  mutate(dam_level_percent = as.numeric(as.character(dam_level_percent))) %>%
  select(
    Year = year,
    Month = month,
    Dam_Level_Pct = dam_level_percent,
    Dam_Name = dam_name
  ) %>%
  mutate(Date = make_date(Year, Month, 1)) %>%
  group_by(Date) %>%
  summarise(
    Avg_Dam_Level = mean(Dam_Level_Pct, na.rm = TRUE),
    .groups = 'drop'
  )

# Merge & Engineer
full_data <- left_join(climate_clean, dams_clean, by = "Date")

precip_90th <- quantile(full_data$Precipitation, 0.90, na.rm = TRUE)

full_data <- full_data %>%
  mutate(
    Season = case_when(
      Month %in% c(12, 1, 2) ~ "Summer",
      Month %in% c(3, 4, 5) ~ "Autumn",
      Month %in% c(6, 7, 8) ~ "Winter",
      TRUE ~ "Spring"
    ),
    Flood_Severity = Precipitation,
    Flood_Event_Flag = if_else(Precipitation > precip_90th, 1, 0),
    Precip_Lag1 = lag(Precipitation, 1),
    Dam_Lag1 = lag(Avg_Dam_Level, 1)
  ) %>%
  drop_na()

write_csv(full_data, "output/processed_data.csv")
