# Q2 — Build spatial analysis dataset
# Inputs: data/raw/merged_q2_spatial_raw.csv
# Outputs:
#   data/processed/dataset_q2_spatial.csv
#   data/processed/q2_lga_summary.csv  (if LGA present)

suppressPackageStartupMessages({
  library(data.table); library(tidyverse); library(janitor); library(here)
})

raw_path <- here("data","raw","merged_q2_spatial_raw.csv")
if (!file.exists(raw_path)) stop("Missing ", raw_path, ". Run 00_load.R first.")
d <- fread(raw_path) %>% clean_names()

# Standardise helpful names
std_rename <- function(dt, old, new) { if (old %in% names(dt)) setnames(dt, old, new); dt }
d <- std_rename(d, "surface_cond_desc", "road_surface")
d <- std_rename(d, "atmosph_cond_desc","weather")
d <- std_rename(d, "lga_name_all",     "lga")
d <- std_rename(d, "rma",              "road_mgmt_auth")
d <- std_rename(d, "postcode_crash",   "postcode")

# Severity as labelled factor (1/2/3 → Fatal/Serious/Minor)
if ("severity" %in% names(d)) {
  if (!is.factor(d$severity)) {
    if (is.numeric(d$severity) || is.integer(d$severity)) {
      d[, severity := factor(severity, levels = c(1,2,3), labels = c("Fatal","Serious","Minor"))]
    } else {
      d[, severity := factor(severity)]
    }
  }
}

# Keep spatially relevant columns (only those that exist)
keep <- intersect(c(
  "accident_no","accident_date","day_of_week",
  "severity","speed_zone","light_condition","weather","road_surface","road_geometry_desc",
  "lga","postcode","road_mgmt_auth","latitude","longitude",
  "vehicle_body_style","vehicle_make"
), names(d))
q2 <- d[, ..keep]

# Clean speed zone & cap
if ("speed_zone" %in% names(q2)) {
  q2[, speed_zone := suppressWarnings(as.numeric(speed_zone))]
  q2[speed_zone %in% c(0,777,888,999), speed_zone := NA_real_]
  q2[, speed_zone := ifelse(is.na(speed_zone), stats::median(speed_zone, na.rm = TRUE), speed_zone)]
  q2 <- q2[speed_zone <= 250]
}

# Keep plausible Vic bbox
if (all(c("latitude","longitude") %in% names(q2))) {
  q2 <- q2[!is.na(latitude) & !is.na(longitude)]
  q2 <- q2[between(latitude, -39.8, -33.0) & between(longitude, 140.0, 151.5)]
}

# Severe flag
if ("severity" %in% names(q2)) q2[, severe01 := fifelse(severity %in% c("Fatal","Serious"), 1L, 0L)]

# Save point-level dataset
dir.create(here("data","processed"), showWarnings = FALSE, recursive = TRUE)
fwrite(q2, here("data","processed","dataset_q2_spatial.csv"))

# LGA aggregates (if LGA available)
if ("lga" %in% names(q2)) {
  lga_summary <- q2[ , .(
    crashes_n = .N,
    severe_n  = sum(severe01, na.rm = TRUE),
    severe_rate = round(mean(severe01, na.rm = TRUE), 4),
    med_speed  = if ("speed_zone" %in% names(q2)) median(speed_zone, na.rm = TRUE) else NA_real_
  ), by = lga][order(-severe_rate, -crashes_n)]
  fwrite(lga_summary, here("data","processed","q2_lga_summary.csv"))
}

cat("Saved dataset(s) to data/processed/\n")
