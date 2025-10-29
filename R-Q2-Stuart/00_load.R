library(tidyverse)
library(janitor)
library(data.table)
# NOTE: This script only loads and MERGES raw data. Do NOT clean, filter, or summarise here.
# Any data cleaning or feature engineering should happen in `01_clean_feature.R`.
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
# 2. Merge all datasets (keeping all records)
# ------------------------------
# Keep all individual records from each table
# Join tables to accident as the base table
merged_all <- accident %>%
    left_join(accident_event,    by = "accident_no") %>%
    left_join(accident_location,  by = "accident_no") %>%
    left_join(atmo,              by = "accident_no") %>%
    left_join(surface,           by = "accident_no") %>%
    left_join(node,              by = "accident_no") %>%
    left_join(sub_dca,           by = "accident_no") %>%
    left_join(person,            by = "accident_no") %>%
    left_join(vehicle,           by = c("accident_no", "vehicle_id"))

# ------------------------------
# 3. Check the result
# ------------------------------
glimpse(merged_all)
nrow(merged_all)

# Save merged dataset (before cleaning)
write.csv(merged_all, file.path("data", "raw", "merged_all_raw.csv"), row.names = FALSE)