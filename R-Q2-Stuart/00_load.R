# Q2 — Load & Merge spatially relevant tables (accident + node + weather + surface + vehicle agg)
# Output: data/raw/merged_q2_spatial_raw.csv  (no cleaning/feature engineering here)

# -----------------------------
# Preflight: required packages
# -----------------------------
required_pkgs <- c("data.table","tidyverse","janitor","here")
missing <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop(
    paste0(
      "Missing packages: ", paste(missing, collapse = ", "),
      "\nInstall with: install.packages(c(\"", paste(missing, collapse="\",\""), "\"))"
    ),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(data.table)
  library(tidyverse)
  library(janitor)
  library(here)
})

cat("Q2 Load —", as.character(Sys.time()), "\n")

# -----------------------------
# Helpers
# -----------------------------
mode_non_na <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (!length(x)) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

first_non_na <- function(x) {
  i <- which(!is.na(x) & x != "")[1]
  if (length(i) && !is.na(i)) x[i] else NA
}

norm_latlon <- function(dt) {
  lat_col <- intersect(names(dt), c("latitude","lat","y"))
  lon_col <- intersect(names(dt), c("longitude","long","lon","x"))
  if (length(lat_col)) setnames(dt, lat_col[1], "latitude")
  if (length(lon_col)) setnames(dt, lon_col[1], "longitude")
  dt
}

# -----------------------------
# 1) Load raw CSVs
# -----------------------------
accident <- fread(here("data","accident.csv")) %>% clean_names()
node     <- fread(here("data","node.csv")) %>% clean_names() %>% norm_latlon()
surface  <- fread(here("data","road_surface_cond.csv")) %>% clean_names()
atmo     <- fread(here("data","atmospheric_cond.csv")) %>% clean_names()
vehicle  <- tryCatch(fread(here("data","vehicle.csv")) %>% clean_names(), error = function(e) NULL)

# -----------------------------
# 2) Collapse each table to 1 row per accident_no
# -----------------------------

# accident may (rarely) have dup rows; collapse by first_non_na across all columns
accident_agg <- accident %>%
  group_by(accident_no) %>%
  summarise(across(everything(), first_non_na), .groups = "drop")

# node often has multiple rows per accident; take median lat/lon + modal admin fields
node_agg <- node %>%
  group_by(accident_no) %>%
  summarise(
    latitude        = suppressWarnings(median(as.numeric(latitude),  na.rm = TRUE)),
    longitude       = suppressWarnings(median(as.numeric(longitude), na.rm = TRUE)),
    lga_name_all    = mode_non_na(lga_name_all),
    postcode_crash  = mode_non_na(postcode_crash),
    .groups = "drop"
  )

# surface & atmo: modal description per accident
surface_agg <- surface %>%
  group_by(accident_no) %>%
  summarise(surface_cond_desc = mode_non_na(surface_cond_desc), .groups = "drop")

atmo_agg <- atmo %>%
  group_by(accident_no) %>%
  summarise(atmosph_cond_desc = mode_non_na(atmosph_cond_desc), .groups = "drop")

# vehicle: counts + modal body/make (if file present)
veh_agg <- tryCatch({
  vehicle %>%
    group_by(accident_no) %>%
    summarise(
      vehicles_n         = dplyr::n_distinct(vehicle_id),
      vehicle_body_style = mode_non_na(vehicle_body_style),
      vehicle_make       = mode_non_na(vehicle_make),
      .groups = "drop"
    )
}, error = function(e) NULL)

# -----------------------------
# 3) Merge (accident as base)
# -----------------------------
merged <- accident_agg %>%
  left_join(node_agg,    by = "accident_no") %>%
  left_join(surface_agg, by = "accident_no") %>%
  left_join(atmo_agg,    by = "accident_no")

if (!is.null(veh_agg)) merged <- merged %>% left_join(veh_agg, by = "accident_no")

cat("Merged rows x cols (pre-repair):", nrow(merged), "x", ncol(merged), "\n")

# -----------------------------
# 4) Safety net: if duplicates remain, repair to 1 row/accident_no
# -----------------------------
dup_tbl <- merged %>% count(accident_no, name = "n") %>% filter(n > 1)
if (nrow(dup_tbl) > 0) {
  dir.create(here("data","raw"), showWarnings = FALSE, recursive = TRUE)
  fwrite(dup_tbl, here("data","raw","q2_merge_duplicates.csv"))
  message(sprintf("Detected %d duplicated accidents after merge; repairing to 1 row per accident_no.", nrow(dup_tbl)))
  
  # type-aware collapse
  merged <- merged %>%
    group_by(accident_no) %>%
    summarise(
      across(where(is.numeric), ~ suppressWarnings(median(., na.rm = TRUE))),
      across(where(is.logical), ~ any(., na.rm = TRUE)),
      across(where(is.factor),  ~ mode_non_na(as.character(.))),
      across(where(is.character), mode_non_na),
      .groups = "drop"
    )
}

# Assert uniqueness now
stopifnot(nrow(merged) == dplyr::n_distinct(merged$accident_no))

# -----------------------------
# 5) Quick lat/long checks
# -----------------------------
lat_ok <- "latitude"  %in% names(merged)
lon_ok <- "longitude" %in% names(merged)
cat("Missing latitude:", if (lat_ok) sum(is.na(merged$latitude)) else NA_integer_,
    " | Missing longitude:", if (lon_ok) sum(is.na(merged$longitude)) else NA_integer_, "\n")

# -----------------------------
# 6) Save raw merged (no cleaning)
# -----------------------------
dir.create(here("data","raw"), showWarnings = FALSE, recursive = TRUE)
out_path <- here("data","raw","merged_q2_spatial_raw.csv")
fwrite(merged, out_path)
cat("Saved:", out_path, "\n")
