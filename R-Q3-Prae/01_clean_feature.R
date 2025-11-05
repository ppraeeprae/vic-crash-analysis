# ============================================
# 01_clean_feature.R
# Dataset for Question 3 — Environmental & Behavioural Interaction
# Input  : merged_with_features.csv  (created by 00_load.R)
# Output : dataset_q3_interaction.csv
# ============================================

# 0) Working directory ----------------------------------------------------
library(here)
setwd(here::here())
getwd()

# 1) Packages -------------------------------------------------------------
library(tidyverse)
library(janitor)
library(stringr)
library(lubridate)
library(forcats)

# 2) Load the merged data -------------------------------------------------
# NOTE: 00_load.R must have been run before this
merged <- readr::read_csv("merged_with_features.csv") %>% clean_names()

# 3) Helper functions -----------------------------------------------------
# 3a. map severity text/labels into 3 levels + binary severe flag
derive_severity <- function(x) {
  x_low <- tolower(as.character(x))
  case_when(
    str_detect(x_low, "fatal")                     ~ "Fatal",
    str_detect(x_low, "serious")                   ~ "Serious injury",
    str_detect(x_low, "other|minor|non")           ~ "Other injury",
    TRUE                                           ~ "Other injury"
  )
}

# 3b. weather grouping (atmosph_cond_desc)
derive_weather <- function(x) {
  x_low <- tolower(x)
  case_when(
    is.na(x_low)                                   ~ "unknown",
    str_detect(x_low, "rain|shower|storm|hail")    ~ "rain/storm",
    str_detect(x_low, "fog|mist|smoke")            ~ "fog/mist",
    str_detect(x_low, "wind")                      ~ "windy",
    str_detect(x_low, "fine|clear")                ~ "fine/clear",
    TRUE                                           ~ "other"
  )
}

# 3c. road surface grouping (surface_cond_desc)
derive_surface <- function(x) {
  x_low <- tolower(x)
  case_when(
    is.na(x_low)                                   ~ "unknown",
    str_detect(x_low, "wet|water|flood")           ~ "wet",
    str_detect(x_low, "dry")                       ~ "dry",
    str_detect(x_low, "loose|gravel|mud|slush")    ~ "loose/other",
    TRUE                                           ~ "other"
  )
}

# 3d. light condition
# Your merged data shows `light_condition` as numeric codes (1..6).
# Use a robust mapping (common for Vic crash data); fallback to "other/unknown".
map_light_num <- function(code) {
  case_when(
    is.na(code)            ~ "unknown",
    code == 1              ~ "daylight",
    code == 2              ~ "dawn",
    code == 3              ~ "dusk",
    code == 4              ~ "dark_street_lights_on",
    code == 5              ~ "dark_street_lights_off",
    code == 6              ~ "dark_no_street_lights",
    TRUE                   ~ "other"
  )
}

# 3e. age group collapsing from person agg (common_age_group)
collapse_age <- function(x) {
  x_low <- tolower(as.character(x))
  case_when(
    is.na(x_low) ~ "unknown",
    str_detect(x_low, "0-1[6-9]|1[7-9]-2[4-5]|17-25|young") ~ "younger",
    str_detect(x_low, "26-39|25-39|adult")                  ~ "adult",
    str_detect(x_low, "40-59|40-64|middle")                 ~ "middle",
    str_detect(x_low, "60|70|80|older|senior")              ~ "older",
    TRUE                                                    ~ "other"
  )
}

# 4) Derive / clean variables ---------------------------------------------
q3 <- merged %>%
  mutate(
    # --- severity --------------------------------------------------------
    # If `severity` is numeric in your data, this will default to "Other injury".
    # That's OK for Q3, because your 00_load.R uses `summary(merged_data$severity)`
    # only to confirm the target exists. If you have a labelled text column, map it here.
    severity_clean   = derive_severity(severity),
    is_severe        = if_else(severity_clean %in% c("Fatal", "Serious injury"), 1L, 0L),
    
    # --- environmental ---------------------------------------------------
    weather_clean    = derive_weather(atmosph_cond_desc),
    surface_clean    = derive_surface(surface_cond_desc),
    light_clean      = map_light_num(light_condition),
    
    # --- behavioural / demographic --------------------------------------
    age_group_clean  = collapse_age(common_age_group),
    sex_common       = forcats::fct_na_value_to_level(as.factor(common_sex), level = "unknown"),
    
    # --- vehicle factors -------------------------------------------------
    vehicle_type_main  = forcats::fct_na_value_to_level(as.factor(common_vehicle_type), level = "unknown"),
    vehicle_age_avg    = vehicle_age_avg,  # from 00_load.R (accident_year - avg_vehicle_year)
    
    # --- interaction-friendly flags -------------------------------------
    is_wet_weather   = if_else(weather_clean %in% c("rain/storm", "fog/mist"), 1L, 0L),
    is_wet_surface   = if_else(surface_clean == "wet", 1L, 0L),
    is_night         = if_else(light_clean %in% c("dark_street_lights_on",
                                                  "dark_street_lights_off",
                                                  "dark_no_street_lights"), 1L, 0L),
    is_dusk_dawn     = if_else(light_clean %in% c("dusk", "dawn"), 1L, 0L),
    is_young_driver  = if_else(age_group_clean == "younger", 1L, 0L),
    is_male_common   = if_else(tolower(as.character(sex_common)) %in% c("m", "male"), 1L, 0L),
    
    # --- actual interaction terms for Q3 --------------------------------
    wet_x_night      = is_wet_surface * is_night,
    rain_x_young     = is_wet_weather * is_young_driver,
    night_x_male     = is_night * is_male_common,
    
    # For quick stratified summaries if needed
    env_risk_combo   = paste(weather_clean, surface_clean, light_clean, sep = "_")
  )

# 5) Keep only columns relevant to Question 3 -----------------------------
q3_export <- q3 %>%
  transmute(
    accident_no,
    
    # target(s)
    severity_clean,
    is_severe,
    
    # environmental
    weather_clean,
    surface_clean,
    light_clean,
    
    # behavioural / demographic
    age_group_clean,
    sex_common,
    
    # vehicle
    vehicle_type_main,
    vehicle_age_avg,
    
    # counts from 00_load.R (can help explain multiple occupants/vehicles)
    n_vehicles,
    n_persons,
    
    # interaction flags
    is_wet_weather,
    is_wet_surface,
    is_night,
    is_dusk_dawn,
    is_young_driver,
    is_male_common,
    wet_x_night,
    rain_x_young,
    night_x_male,
    env_risk_combo,
    
    # keep the original engineered index as well (from 00_load.R)
    weather_road_index
  )

# 6) Save -----------------------------------------------------------------
readr::write_csv(q3_export, "dataset_q3_interaction.csv")

cat("dataset_q3_interaction.csv created.\nRows:", nrow(q3_export),
    "\nCols:", ncol(q3_export), "\n")
