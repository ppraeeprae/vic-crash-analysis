suppressPackageStartupMessages({
    library(tidyverse)
    library(janitor)
    library(data.table)
    library(here)
})

# NOTE: This script only loads and MERGES raw data. Do NOT clean, filter, or summarise here.
# Any data cleaning or feature engineering should happen in `01_clean_feature.R` or other scripts.

# 1) Load all datasets
accident          <- fread(here("data", "accident.csv")) %>% clean_names()
accident_event    <- fread(here("data", "accident_event.csv")) %>% clean_names()
accident_location <- fread(here("data", "accident_location.csv")) %>% clean_names()
atmo              <- fread(here("data", "atmospheric_cond.csv")) %>% clean_names()
node              <- fread(here("data", "node.csv")) %>% clean_names()
person            <- fread(here("data", "person.csv")) %>% clean_names()
surface           <- fread(here("data", "road_surface_cond.csv")) %>% clean_names()
sub_dca           <- fread(here("data", "sub_dca.csv")) %>% clean_names()
vehicle           <- fread(here("data", "vehicle.csv")) %>% clean_names()

# 2) Check data dimensions
data_dims <- tibble(
    dataset = c("accident", "accident_event", "accident_location", "atmo",
                            "node", "person", "surface", "sub_dca", "vehicle"),
    rows = c(nrow(accident), nrow(accident_event), nrow(accident_location),
                     nrow(atmo), nrow(node), nrow(person), nrow(surface),
                     nrow(sub_dca), nrow(vehicle)),
    columns = c(ncol(accident), ncol(accident_event), ncol(accident_location),
                            ncol(atmo), ncol(node), ncol(person), ncol(surface),
                            ncol(sub_dca), ncol(vehicle))
)
print(data_dims)

# 3) Merge all datasets (keeping all records). Accident table is the base.
merged_all <- accident %>%
    left_join(accident_event,    by = "accident_no") %>%
    left_join(accident_location, by = "accident_no") %>%
    left_join(atmo,              by = "accident_no") %>%
    left_join(surface,           by = "accident_no") %>%
    left_join(node,              by = "accident_no") %>%
    left_join(sub_dca,           by = "accident_no") %>%
    left_join(person,            by = "accident_no") %>%
    left_join(vehicle,           by = c("accident_no", "vehicle_id"))

# 4) Check merged dataset
cat("Merged dataset dimensions:\n")
cat("Rows:", nrow(merged_all), "\n")
cat("Columns:", ncol(merged_all), "\n\n")
cat("First few columns and types:\n")
glimpse(merged_all)

# 5) Missingness overview
missing_summary <- merged_all %>%
    summarise(across(everything(), ~sum(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "column", values_to = "missing_count") %>%
    mutate(pct_missing = round(missing_count / nrow(merged_all) * 100, 2)) %>%
    filter(missing_count > 0) %>%
    arrange(desc(missing_count))

cat("Top 20 columns by missing count:\n")
print(head(missing_summary, 20))

# 5b) Targeted missingness (vehicle-related rows only)
pct_na <- function(v) {
    if (is.numeric(v)) round(mean(is.na(v)) * 100, 2) else round(mean(is.na(v) | v == "") * 100, 2)
}

nonveh <- merged_all %>% filter(!is.na(vehicle_id) & vehicle_id != "")

veh_missing <- tibble(
    column = c("vehicle_power", "carry_capacity", "vehicle_weight", "cubic_capacity", "vehicle_year_manuf"),
    pct_missing_vehicle_rows = c(
        pct_na(nonveh$vehicle_power),
        pct_na(nonveh$carry_capacity),
        pct_na(nonveh$vehicle_weight),
        pct_na(nonveh$cubic_capacity),
        pct_na(nonveh$vehicle_year_manuf)
    )
)

cat("\nVehicle-related missingness (only rows with vehicle_id):\n")
print(veh_missing)

# 6) Save merged dataset
raw_dir <- here("data", "raw")
if (!dir.exists(raw_dir)) dir.create(raw_dir, recursive = TRUE)

out_path <- here("data", "raw", "merged_all_raw.csv")
write.csv(merged_all, out_path, row.names = FALSE)

cat("\nMerged dataset saved to:", out_path, "\n")
cat("Total rows:", nrow(merged_all), "\n")
cat("Total columns:", ncol(merged_all), "\n")