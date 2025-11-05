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
# NOTE: Use the unified raw merge produced by R-Q1-Ploy/00_load.R
#       Path: data/raw/merged_all_raw.csv
merged <- readr::read_csv(here("data", "raw", "merged_all_raw.csv")) %>% clean_names()

# 3) Helper functions -----------------------------------------------------
# 3a. map severity into 3 levels + binary severe flag (robust to numeric/text)
derive_severity <- function(x) {
  # try numeric codes first (common Vic coding: 1=fatal, 2=serious, 3=other, 4=PDO)
  x_num <- suppressWarnings(as.numeric(as.character(x)))
  out <- rep(NA_character_, length(x))
  if (any(!is.na(x_num))) {
    out[!is.na(x_num) & x_num == 1] <- "Fatal"
    out[!is.na(x_num) & x_num == 2] <- "Serious injury"
    out[!is.na(x_num) & x_num %in% c(3,4)] <- "Other injury"
  }
  # fill remaining using text detection
  idx_na <- is.na(out)
  if (any(idx_na)) {
    x_low <- tolower(as.character(x))[idx_na]
    out[idx_na] <- case_when(
      str_detect(x_low, "fatal")                     ~ "Fatal",
      str_detect(x_low, "serious")                   ~ "Serious injury",
      str_detect(x_low, "other|minor|non|pdo|damage")~ "Other injury",
      TRUE                                            ~ "Other injury"
    )
  }
  out
}

  # 3x. simple helpers for accident-level aggregation -----------------------
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

# 3x. simple helpers for accident-level aggregation -----------------------
first_non_na <- function(x) {
  x[which(!is.na(x) & x != "")[1]]
}

mode_char <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (!length(x)) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

# 3x.1 standardize key columns that may vary in name across sources
take_first_existing <- function(df, candidates) {
  for (nm in candidates) {
    if (nm %in% names(df)) return(df[[nm]])
  }
  return(rep(NA, nrow(df)))
}

# Create standardized columns used downstream regardless of original names
merged <- merged %>%
  mutate(
    accident_date_std         = take_first_existing(., c("accident_date", "crash_date", "date")),
    light_condition_num_std   = suppressWarnings(as.numeric(take_first_existing(., c("light_condition", "light_cond")))),
    light_condition_desc_std  = take_first_existing(., c("light_condition_desc", "light_cond_desc")),
    atmosph_cond_desc_std     = take_first_existing(., c("atmosph_cond_desc", "atmospheric_cond_desc", "atmospheric_condition_desc")),
    surface_cond_desc_std     = take_first_existing(., c("surface_cond_desc", "road_surface_desc", "road_surface_condition_desc")),
    person_id_std             = take_first_existing(., c("person_id", "personid")),
    vehicle_id_std            = take_first_existing(., c("vehicle_id", "vehicleid")),
    road_user_type_std        = suppressWarnings(as.numeric(take_first_existing(., c("road_user_type", "road_user_type_code")))),
    sex_std                   = take_first_existing(., c("sex", "gender")),
    age_group_std             = take_first_existing(., c("age_group", "agegroup")),
    vehicle_year_manuf_std    = suppressWarnings(as.numeric(take_first_existing(., c("vehicle_year_manuf", "vehicle_year", "year_manufactured")))),
    vehicle_body_style_std    = take_first_existing(., c("vehicle_body_style", "body_style", "vehicle_type")),
    accident_year_std         = {
      d <- suppressWarnings(lubridate::parse_date_time(accident_date_std, orders = c("Y-m-d","d/m/Y","Ymd","m/d/Y","d-b-Y","d-B-Y")))
      suppressWarnings(as.numeric(lubridate::year(d)))
    }
  )

# 3y. Build accident-level dataset from merged rows -----------------------
# Environmental (take the first non-missing per accident)
acc_env <- merged %>%
  group_by(accident_no) %>%
  summarise(
    severity              = suppressWarnings(first_non_na(severity)),
    atmosph_cond_desc     = first_non_na(atmosph_cond_desc_std),
    surface_cond_desc     = first_non_na(surface_cond_desc_std),
    light_condition_num   = suppressWarnings(as.numeric(first_non_na(light_condition_num_std))),
    light_condition_desc  = first_non_na(light_condition_desc_std),
    accident_year         = suppressWarnings(as.numeric(first_non_na(accident_year_std))),
    .groups = "drop"
  )

# Vehicles: distinct by accident_no, vehicle_id to avoid duplicated joins
veh_sub <- merged %>%
  filter(!is.na(vehicle_id_std) & vehicle_id_std != "") %>%
  distinct(accident_no, vehicle_id_std, .keep_all = TRUE)

veh_agg <- veh_sub %>%
  mutate(
    vehicle_year_manuf = suppressWarnings(as.numeric(vehicle_year_manuf_std)),
    vehicle_body_style = as.character(vehicle_body_style_std)
  ) %>%
  group_by(accident_no) %>%
  summarise(
    n_vehicles          = n_distinct(vehicle_id_std),
    common_vehicle_type = mode_char(vehicle_body_style),
    avg_vehicle_year    = suppressWarnings(mean(vehicle_year_manuf, na.rm = TRUE)),
    .groups = "drop"
  )

# Persons: distinct by accident_no, person_id; drivers only where possible
person_sub <- merged %>%
  filter(!is.na(person_id_std) & person_id_std != "") %>%
  distinct(accident_no, person_id_std, .keep_all = TRUE)

drv_only <- person_sub %>%
  mutate(road_user_type_std = suppressWarnings(as.numeric(road_user_type_std))) %>%
  filter(is.na(road_user_type_std) | road_user_type_std == 2)  # if missing codes, keep all as fallback

pers_agg <- drv_only %>%
  mutate(
    sex       = as.character(sex_std),
    age_group = as.character(age_group_std)
  ) %>%
  group_by(accident_no) %>%
  summarise(
    n_persons          = n_distinct(person_id_std),
    common_sex         = mode_char(sex),
    common_age_group   = mode_char(age_group),
    .groups = "drop"
  )

# Join all accident-level pieces
acc <- acc_env %>%
  left_join(veh_agg,  by = "accident_no") %>%
  left_join(pers_agg, by = "accident_no")

# 4) Derive / clean variables ---------------------------------------------
q3 <- acc %>%
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
    light_clean      = dplyr::if_else(!is.na(light_condition_num),
                                      map_light_num(light_condition_num),
                                      {
                                        lcd <- tolower(as.character(light_condition_desc))
                                        case_when(
                                          str_detect(lcd, "day|sun|light") ~ "daylight",
                                          str_detect(lcd, "dawn") ~ "dawn",
                                          str_detect(lcd, "dusk") ~ "dusk",
                                          str_detect(lcd, "street.*on") ~ "dark_street_lights_on",
                                          str_detect(lcd, "street.*off") ~ "dark_street_lights_off",
                                          str_detect(lcd, "dark|night") ~ "dark_no_street_lights",
                                          TRUE ~ "other"
                                        )
                                      }
    ),
    
    # --- behavioural / demographic --------------------------------------
  age_group_clean  = collapse_age(common_age_group),
  sex_common       = forcats::fct_na_value_to_level(as.factor(common_sex), level = "unknown"),
    
    # --- vehicle factors -------------------------------------------------
  vehicle_type_main  = forcats::fct_na_value_to_level(as.factor(common_vehicle_type), level = "unknown"),
  vehicle_age_avg    = ifelse(!is.na(accident_year) & !is.na(avg_vehicle_year),
                accident_year - avg_vehicle_year, NA_real_),
    
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
    env_risk_combo   = paste(weather_clean, surface_clean, light_clean, sep = "_"),
    # simple index to replace earlier engineered value (if any)
    weather_road_index = coalesce(is_wet_weather + is_wet_surface, 0L)
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
    
    # engineered index derived here
    weather_road_index
  )

# 6) Save -----------------------------------------------------------------
readr::write_csv(q3_export, "dataset_q3_interaction.csv")

cat("dataset_q3_interaction.csv created.\nRows:", nrow(q3_export),
    "\nCols:", ncol(q3_export), "\n")
