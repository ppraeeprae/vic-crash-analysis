# =========================
# 0) Working directory
# =========================
# The simplest way: select the data folder directly
library(here)
setwd(here::here())        # go to project root
getwd()                 # Screenshot 1: Shows the successfully set working directory path

# =========================
# 1) Packages
# =========================
library(tidyverse)
library(readr)
library(dplyr)
library(janitor)
library(lubridate)
library(stringr)

# =========================
# 2) Read CSV files
# =========================
accident       <- read_csv("data/accident.csv")
acc_event      <- read_csv("data/accident_event.csv")
acc_location   <- read_csv("data/accident_location.csv")
atmos          <- read_csv("data/atmospheric_cond.csv")
node           <- read_csv("data/node.csv")
person         <- read_csv("data/person.csv")
road_surface   <- read_csv("data/road_surface_cond.csv")
sub_dca        <- read_csv("data/sub_dca.csv")
vehicle        <- read_csv("data/vehicle.csv")

ls()                      # Check that all data objects are loaded
glimpse(accident)         # Screenshot 2: Example of one dataset before cleaning

# =========================
# 3) Clean column names (standardize naming)
# =========================
accident      <- clean_names(accident)
acc_event     <- clean_names(acc_event)
acc_location  <- clean_names(acc_location)
atmos         <- clean_names(atmos)
node          <- clean_names(node)
person        <- clean_names(person)
road_surface  <- clean_names(road_surface)
sub_dca       <- clean_names(sub_dca)
vehicle       <- clean_names(vehicle)

names(accident)[1:10]     # Expect to see: accident_no, accident_date, ...

# =========================
# 4) Aggregate one-to-many tables into per-accident summaries
#    (Prevents row multiplication when joining)
# =========================
acc_event_agg <- acc_event %>%
  group_by(accident_no) %>%
  summarise(event_types = paste(unique(event_type), collapse = "; "),
            .groups = "drop")

veh_agg <- vehicle %>%
  group_by(accident_no) %>%
  summarise(
    n_vehicles = n(),
    common_vehicle_type = names(sort(table(vehicle_type), decreasing = TRUE))[1],
    avg_vehicle_year = mean(vehicle_year_manuf, na.rm = TRUE),
    .groups = "drop"
  )

# Note: Some accidents may not contain sex or age_group values. Handle NULL cases.
per_agg <- person %>%
  group_by(accident_no) %>%
  summarise(
    n_persons = n(),
    common_sex = ifelse(length(na.omit(sex)) == 0, NA_character_,
                        names(sort(table(na.omit(sex)), decreasing = TRUE))[1]),
    common_age_group = ifelse(length(na.omit(age_group)) == 0, NA_character_,
                              names(sort(table(na.omit(age_group)), decreasing = TRUE))[1]),
    .groups = "drop"
  )

# =========================
# 5) Master join (accident = base table)
#    Note: Many-to-many joins are expected since some accidents have multiple rows
#          in atmospheric / road surface / sub-DCA tables.
# =========================
merged_data <- accident %>%
  left_join(node,         by = "accident_no") %>%
  left_join(atmos,        by = "accident_no", relationship = "many-to-many") %>%
  left_join(road_surface, by = "accident_no", relationship = "many-to-many") %>%
  left_join(acc_location, by = "accident_no") %>%
  left_join(sub_dca,      by = "accident_no", relationship = "many-to-many") %>%
  left_join(acc_event_agg,by = "accident_no") %>%
  left_join(veh_agg,      by = "accident_no") %>%
  left_join(per_agg,      by = "accident_no")

nrow(merged_data)         # Should be around ~271,856
glimpse(merged_data)      # Screenshot 3: Structure after merging

# =========================
# 6) Basic cleaning
# =========================
# 6a. Safely drop duplicate node_id columns if they exist
dup_cols <- c("node_id.x", "node_id.y")
dup_cols <- dup_cols[dup_cols %in% names(merged_data)]

if (length(dup_cols) > 0) {
  merged_data <- merged_data %>% select(-all_of(dup_cols))
}

# 6b. Check missing latitude/longitude
sum(is.na(merged_data$latitude) | is.na(merged_data$longitude))  # Screenshot 4: Missing coordinates count

# 6c. Fill missing coords with mean so we don't lose rows downstream
merged_data$latitude[is.na(merged_data$latitude)]   <- mean(merged_data$latitude,  na.rm = TRUE)
merged_data$longitude[is.na(merged_data$longitude)] <- mean(merged_data$longitude, na.rm = TRUE)

summary(merged_data$severity)   # Screenshot 5: Target variable ready for modeling

# =========================
# 7) Minimal Feature Engineering used in the report
# =========================
merged_data <- merged_data %>%
  mutate(
    # Crash time period
    accident_hour = hour(accident_time),
    crash_time_band = case_when(
      accident_hour >= 6  & accident_hour < 10 ~ "Morning peak",
      accident_hour >= 10 & accident_hour < 16 ~ "Day",
      accident_hour >= 16 & accident_hour < 19 ~ "Evening peak",
      TRUE ~ "Night"
    ),
    # Average vehicle age per accident
    accident_year = year(ymd(accident_date)),
    vehicle_age_avg = ifelse(is.finite(avg_vehicle_year),
                             accident_year - round(avg_vehicle_year),
                             NA_real_),
    # Environmental risk index
    weather_road_index = as.integer(
      str_detect(tolower(atmosph_cond_desc), "rain|wet|shower|fog|storm") |
        str_detect(tolower(surface_cond_desc), "wet|slippery")
    ),
    # Convert key variables into factors
    severity = as.factor(severity),
    common_age_group = as.factor(common_age_group),
    common_sex = as.factor(common_sex)
  )

glimpse(merged_data %>% select(severity, crash_time_band, vehicle_age_avg,
                               weather_road_index, n_vehicles, n_persons) %>% head(10))
# Screenshot 6: Display of newly created feature columns

# =========================
# 8) Save final datasets
# =========================
write_csv(merged_data, "merged_victorian_crash_data.csv")       # Basic merged dataset
write_csv(merged_data, "merged_with_features.csv")              # Dataset with engineered features

# =========================
# 9) Summary statistics for report
# =========================
cat(
  "\nRows:", nrow(merged_data),
  "\nCols:", ncol(merged_data),
  "\nMissing coords after fill:", sum(is.na(merged_data$latitude) | is.na(merged_data$longitude)), "\n"
)
