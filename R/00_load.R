library(tidyverse)
library(janitor)
library(data.table)
# ------------------------------
# 1. Load all datasets
# ------------------------------
accident         <- fread(file.path("data", "accident.csv")) %>% clean_names()
accident_event   <- fread(file.path("data", "accident_event.csv")) %>% clean_names()
accident_location<- fread(file.path("data", "accident_location.csv")) %>% clean_names()
atmo             <- fread(file.path("data", "atmospheric_cond.csv")) %>% clean_names()
node             <- fread(file.path("data", "node.csv")) %>% clean_names()
person           <- fread(file.path("data", "person.csv")) %>% clean_names()
surface          <- fread(file.path("data", "road_surface_cond.csv")) %>% clean_names()
sub_dca          <- fread(file.path("data", "sub_dca.csv")) %>% clean_names()
vehicle          <- fread(file.path("data", "vehicle.csv")) %>% clean_names()

# ------------------------------
# 2. Summarise each supporting table
# ------------------------------

## Person: get driver demographics
person_summary <- person %>%
    group_by(accident_no) %>%
    summarise(
        mean_age_group = first(age_group),
        pct_male = mean(sex == "M", na.rm = TRUE),
        main_injury = first(inj_level_desc),
        .groups = "drop"
    )

## Vehicle: summarise vehicle details
veh_summary <- vehicle %>%
    group_by(accident_no) %>%
    summarise(
        main_vehicle_body = first(vehicle_body_style),
        main_vehicle_type = first(vehicle_type),
        main_vehicle_make = first(vehicle_make),
        avg_vehicle_year = mean(vehicle_year_manuf, na.rm = TRUE),
        .groups = "drop"
    )

## Accident event: take first event type
event_summary <- accident_event %>%
    group_by(accident_no) %>%
    summarise(first_event_type = first(event_type_desc), .groups = "drop")

## Accident location: summarise key info
loc_summary <- accident_location %>%
    group_by(accident_no) %>%
    summarise(
        road_type = first(road_type_int),
        direction = first(direction_location),
        .groups = "drop"
    )

## Atmospheric condition: summarise
atmo_summary <- atmo %>%
    group_by(accident_no) %>%
    summarise(weather = first(atmosph_cond_desc), .groups = "drop")

## Road surface condition
surface_summary <- surface %>%
    group_by(accident_no) %>%
    summarise(road_surface = first(surface_cond_desc), .groups = "drop")

## Node (location)
node_summary <- node %>%
    group_by(accident_no) %>%
    summarise(
        lga_name_all = first(lga_name_all),
        latitude = first(latitude),
        longitude = first(longitude),
        postcode_crash = first(postcode_crash),
        .groups = "drop"
    )

## Sub-DCA (lookup)
dca_lookup <- sub_dca %>%
    select(accident_no, sub_dca_code_desc) %>%
    distinct()

# ------------------------------
# 3. Merge all datasets
# ------------------------------
merged_all <- accident %>%
    left_join(event_summary,   by = "accident_no") %>%
    left_join(loc_summary,     by = "accident_no") %>%
    left_join(atmo_summary,    by = "accident_no") %>%
    left_join(node_summary,    by = "accident_no") %>%
    left_join(person_summary,  by = "accident_no") %>%
    left_join(surface_summary, by = "accident_no") %>%
    left_join(veh_summary,     by = "accident_no") %>%
    left_join(dca_lookup,      by = "accident_no")

# ------------------------------
# 4. Check the result
# ------------------------------
glimpse(merged_all)
nrow(merged_all)

# Save merged dataset (before cleaning)
write.csv(merged_all, file.path("data", "raw", "merged_all_raw.csv"), row.names = FALSE)