# Q2 — LGA view: Top-10 severe-rate table + VIC LGA choropleth
# Inputs:  data/processed/dataset_q2_spatial.csv
# Outputs: data/processed/q2_lga_summary.csv
#          models/q2_top10_lga_severerate.csv
#          plots/q2_spatial/q2_top10_lga_severerate.png
#          plots/q2_spatial/q2_lga_choropleth.png   (if ozmaps available)

suppressPackageStartupMessages({
  library(data.table); library(tidyverse); library(here)
})

safe_breaks <- function(x, n = 5) {
  x <- x[is.finite(x)]
  if (!length(x)) return(c(0, 1))
  br <- try(classInt::classIntervals(x, n = n, style = "quantile")$brks, silent = TRUE)
  if (inherits(br, "try-error") || length(unique(br)) < length(br)) {
    rng <- range(x, na.rm = TRUE); if (diff(rng) == 0) rng <- c(rng[1]-0.5, rng[2]+0.5)
    br <- unique(pretty(rng, n)); if (length(br) < 3) br <- seq(rng[1], rng[2], length.out = max(3, n + 1))
  }
  unique(br)
}

# ---------- load ----------
in_csv <- here("data","processed","dataset_q2_spatial.csv")
if (!file.exists(in_csv)) stop("Missing ", in_csv, ". Run 01_clean_feature.R first.")
q2 <- fread(in_csv)

if (!"lga" %in% names(q2)) stop("No 'lga' column found in dataset_q2_spatial.csv")

# ---------- summarise by LGA ----------
lga_summary <- q2[, .(
  crashes_n   = .N,
  severe_n    = sum(severe01, na.rm = TRUE),
  severe_rate = mean(severe01, na.rm = TRUE)
), by = lga][order(-severe_rate, -crashes_n)]

dir.create(here("data","processed"), showWarnings = FALSE, recursive = TRUE)
fwrite(lga_summary, here("data","processed","q2_lga_summary.csv"))

# ---------- Top-10 table & bar chart ----------
dir.create(here("plots","q2_spatial"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("models"), showWarnings = FALSE, recursive = TRUE)

min_n <- 200L  # avoid tiny LGAs dominating; tweak as needed
top10 <- lga_summary[crashes_n >= min_n][order(-severe_rate, -crashes_n)][1:10]
fwrite(top10, here("models","q2_top10_lga_severerate.csv"))

p_top <- ggplot(top10, aes(x = reorder(lga, severe_rate), y = severe_rate)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(title = "Top LGAs by severe crash rate (min N = 200)",
       x = NULL, y = "Severe rate") +
  theme_minimal(base_size = 11)
ggsave(here("plots","q2_spatial","q2_top10_lga_severerate.png"),
       p_top, width = 7, height = 5, dpi = 130)

# ---------- Choropleth (requires ozmaps) ----------
if (requireNamespace("ozmaps", quietly = TRUE)) {
  suppressPackageStartupMessages({ library(sf); library(ozmaps); library(classInt); library(dplyr) })
  
  lga_sf <- ozmaps::abs_lga
  
  # Pick a STATE column that exists on your version of ozmaps
  state_candidates <- c("STATE_NAME","STATE_NAME_2016","STE_NAME16","STE_NAME21","STATE","STATE_NAME_2021")
  state_col <- state_candidates[state_candidates %in% names(lga_sf)][1]
  
  if (!is.na(state_col)) {
    # Filter to Victoria by state name/abbr
    lga_sf <- lga_sf %>% dplyr::filter(.data[[state_col]] %in% c("Victoria","VIC"))
  } else {
    # Fallback: spatially clip to the Victoria polygon from ozmap_states
    st_sf <- ozmaps::ozmap_states
    name_col_state <- if ("NAME" %in% names(st_sf)) "NAME" else names(st_sf)[1]
    vic_poly <- st_sf %>% dplyr::filter(.data[[name_col_state]] %in% c("Victoria","VIC")) %>% sf::st_union()
    lga_sf <- sf::st_make_valid(lga_sf)
    lga_sf <- sf::st_intersection(lga_sf, vic_poly)
  }
  
  # Pick an LGA name column that exists
  name_candidates <- c("NAME","LGA_NAME","LGA_NAME16","LGA_NAME_2016","LGA_NAME21","LGA_NAME_2021")
  nm_col <- name_candidates[name_candidates %in% names(lga_sf)][1]
  if (is.na(nm_col)) stop("Could not find an LGA name column in ozmaps::abs_lga")
  
  lga_sf <- lga_sf %>% dplyr::mutate(lga_key = toupper(.data[[nm_col]]))
  lga_sum2 <- lga_summary %>% dplyr::mutate(lga_key = toupper(lga))
  
  vic_map  <- dplyr::left_join(lga_sf, lga_sum2, by = "lga_key")
  
  # Optional: print any unmatched names to help recode
  if (any(is.na(vic_map$severe_rate))) {
    message("Unmatched LGA names (first few):")
    print(utils::head(setdiff(unique(toupper(lga_summary$lga)), unique(lga_sf$lga_key)), 20))
  }
  
  brks <- safe_breaks(vic_map$severe_rate, n = 5)
  
  p_map <- ggplot(vic_map) +
    geom_sf(aes(fill = cut(severe_rate, brks, include.lowest = TRUE)), color = NA) +
    labs(title = "Severe crash rate by LGA (Victoria)", fill = "Severe rate") +
    theme_minimal(base_size = 11)
  
  ggsave(here("plots","q2_spatial","q2_lga_choropleth.png"),
         p_map, width = 7.5, height = 6, dpi = 130)
  
} else {
  message("Install ozmaps to build the LGA map: install.packages('ozmaps')")
}
