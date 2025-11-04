## Q1 Severity Preparation (script version)
## - Loads merged raw dataset
## - Produces row-level and accident-level processed CSVs for modeling

suppressPackageStartupMessages({
	library(tidyverse)
	library(janitor)
	library(data.table)
	library(here)
})

# 1) Load merged dataset created by 00_load.R
merged_path <- here("data", "raw", "merged_all_raw.csv")
if (!file.exists(merged_path)) {
	stop("Missing merged data: ", merged_path, ". Run R-Q1-Ploy/00_load.R first.")
}
merged_all <- fread(merged_path)

# 2) Row-level severity dataset
q1_severity <- merged_all[, .(
	accident_no,
	severity,                 # target variable (1 Fatal, 2 Serious, 3 Minor)
	speed_zone,
	light_condition,
	weather = atmosph_cond_desc,         # from atmospheric_cond.csv
	road_surface = surface_cond_desc,    # from road_surface_cond.csv
	road_geometry_desc,
	main_vehicle_body = vehicle_body_style,
	main_vehicle_make = vehicle_make,
	age_group                            # driver/person age group
)]

# Clean speed zone data
q1_severity[, speed_zone := as.numeric(speed_zone)]
q1_severity[speed_zone %in% c(0, 777, 888, 999), speed_zone := NA_real_]

# Handle missing values in weather and road_surface
q1_severity[, weather := fifelse(is.na(weather), "Unknown", weather)]
q1_severity[, road_surface := fifelse(is.na(road_surface), "Unknown", road_surface)]

# Convert severity to factor with meaningful labels
q1_severity[, severity := factor(severity, levels = c(1, 2, 3), labels = c("Fatal", "Serious", "Minor"))]

# Impute missing speed_zone with median of available
q1_severity[, speed_zone := ifelse(is.na(speed_zone), median(speed_zone, na.rm = TRUE), speed_zone)]

# Cap speeds to realistic limits (<= 250 km/h)
q1_severity_clean <- q1_severity[speed_zone <= 250]

# Save row-level dataset
proc_dir <- here("data", "processed")
if (!dir.exists(proc_dir)) dir.create(proc_dir, recursive = TRUE)
fwrite(q1_severity_clean, here("data", "processed", "q1_severity_clean.csv"))

# 3) Accident-level feature set (one row per accident)
first_non_na <- function(x) x[which(!is.na(x) & x != "")[1]]

# Read raw person/vehicle tables to avoid duplicate inflation from fully merged rows
dt_vehicle <- fread(here("data", "vehicle.csv"))
dt_person  <- fread(here("data", "person.csv"))

# Ensure raw tables use snake_case column names to match references below
# (e.g., ROAD_USER_TYPE -> road_user_type, VEHICLE_MAKE -> vehicle_make)
setnames(dt_vehicle, janitor::make_clean_names(names(dt_vehicle)))
setnames(dt_person,  janitor::make_clean_names(names(dt_person)))

# Base environmental features from merged_all
acc_env <- merged_all[, .(
	severity = first(severity),
	speed_zone = suppressWarnings(as.numeric(first_non_na(speed_zone))),
	light_condition = first_non_na(light_condition),
	weather = first_non_na(atmosph_cond_desc),
	road_surface = first_non_na(surface_cond_desc),
	road_geometry_desc = first_non_na(road_geometry_desc)
), by = accident_no]

# Vehicle aggregates from raw vehicle table
veh_agg <- dt_vehicle[, .(
	vehicles_n = uniqueN(na.omit(vehicle_id)),
	main_vehicle_body = if (!all(is.na(vehicle_body_style))) names(sort(table(vehicle_body_style), decreasing = TRUE))[1] else NA_character_,
	main_vehicle_make = if (!all(is.na(vehicle_make))) names(sort(table(vehicle_make), decreasing = TRUE))[1] else NA_character_
), by = accident_no]

# Driver-only aggregates from raw person table (road_user_type == 2)
drv_agg <- dt_person[road_user_type == 2, .(
	drivers_n = .N,
	driver_pct_male = mean(sex == "M", na.rm = TRUE),
	driver_age_group_mode = if (!all(is.na(age_group))) names(sort(table(age_group), decreasing = TRUE))[1] else NA_character_
), by = accident_no]

# Join all pieces
acc_df <- acc_env[veh_agg, on = "accident_no"][drv_agg, on = "accident_no"]

# Clean speed zone codes and cap
acc_df[, speed_zone := as.numeric(speed_zone)]
acc_df[speed_zone %in% c(0, 777, 888, 999), speed_zone := NA_real_]
acc_df[, speed_zone := ifelse(is.na(speed_zone), median(speed_zone, na.rm = TRUE), speed_zone)]
acc_df <- acc_df[speed_zone <= 250]

# Factor target with labels
acc_df[, severity := factor(severity, levels = c(1, 2, 3), labels = c("Fatal", "Serious", "Minor"))]

# Save accident-level dataset
fwrite(acc_df, here("data", "processed", "q1_severity_accident.csv"))

cat("Saved:\n - ", here("data", "processed", "q1_severity_clean.csv"),
		"\n - ", here("data", "processed", "q1_severity_accident.csv"), "\n")
