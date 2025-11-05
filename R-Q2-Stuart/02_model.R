# Q2 — Spatial hotspot analysis (Gi*) + grid LISA
# Inputs: data/processed/dataset_q2_spatial.csv
# Outputs: plots/q2_spatial/*.png, models/q2_hotspots_top.csv, models/q2_local_moran_grid.csv

suppressPackageStartupMessages({
  library(data.table); library(tidyverse); library(here)
  library(sf); library(spdep); library(classInt); library(hexbin)
})

set.seed(42)  # for reproducible jitter & sampling

proc_csv <- here("data","processed","dataset_q2_spatial.csv")
if (!file.exists(proc_csv)) stop("Missing ", proc_csv, ". Run 01_clean_feature.R first.")
q2 <- fread(proc_csv)

stopifnot(all(c("latitude","longitude") %in% names(q2)))
if (!"severe01" %in% names(q2)) q2[, severe01 := ifelse(severity %in% c("Fatal","Serious"), 1L, 0L)]

# -----------------------------
# Helpers
# -----------------------------
safe_breaks <- function(x, n = 5) {
  x <- x[is.finite(x)]
  if (!length(x)) return(c(0, 1))
  ux <- unique(x)
  # If too few unique values or quantiles collide, fall back to pretty/equal
  brks <- try(classInt::classIntervals(x, n = n, style = "quantile")$brks, silent = TRUE)
  if (inherits(brks, "try-error") || length(unique(brks)) < length(brks)) {
    rng <- range(x, na.rm = TRUE)
    if (diff(rng) == 0) rng <- c(rng[1] - 0.5, rng[2] + 0.5)
    brks <- unique(pretty(rng, n))
    if (length(brks) < 3) brks <- seq(rng[1], rng[2], length.out = max(3, n + 1))
  }
  unique(brks)
}

# Tiny jitter (in projected units) only for neighbor search when identical points exist
jitter_coords <- function(M, sd = 0.05) {
  # M is a numeric matrix (x,y)
  dup <- duplicated(round(M, 3)) | duplicated(round(M, 3), fromLast = TRUE)
  if (any(dup)) {
    Mj <- M
    Mj[dup, ] <- Mj[dup, ] + matrix(rnorm(sum(dup) * 2, sd = sd), ncol = 2)
    return(Mj)
  }
  M
}

# -----------------------------
# 1) Project points (EPSG:3111 preferred) & set up dirs
# -----------------------------
pts <- st_as_sf(q2, coords = c("longitude","latitude"), crs = 4326, remove = FALSE)
suppressWarnings({ pts_3111 <- tryCatch(st_transform(pts, 3111), error = function(e) st_transform(pts, 3857)) })

dir.create(here("plots","q2_spatial"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("models"), recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 2) Density (hexbin)
# -----------------------------
p_hex <- ggplot(st_drop_geometry(pts), aes(longitude, latitude)) +
  stat_bin_hex(bins = 60) + coord_equal() +
  labs(title = "Crash density across Victoria (hexbin)", x = "Longitude", y = "Latitude") +
  theme_minimal(base_size = 11)
ggsave(here("plots","q2_spatial","q2_hex_density.png"), p_hex, width = 7.5, height = 6, dpi = 130)

# -----------------------------
# 3) Point-level Getis–Ord Gi* on severe01 using 8-NN (with jitter for identical coords)
# -----------------------------
coords <- sf::st_coordinates(pts_3111)
coords_j <- jitter_coords(coords, sd = 0.05)  # ~5 cm jitter only for NB graph

knn <- spdep::knearneigh(coords_j, k = 8)
nb  <- spdep::knn2nb(knn)
lw  <- spdep::nb2listw(nb, style = "B")

gi <- spdep::localG(as.numeric(q2$severe01), lw)
q2$gi_z <- as.numeric(gi)

# Save top hotspots
fwrite(q2[order(-gi_z)][, .(accident_no, latitude, longitude, severity, gi_z)][1:500],
       here("models","q2_hotspots_top.csv"))

# Hotspot quick plot (z > 2)
pts$gi_z <- q2$gi_z
hot <- pts %>% filter(gi_z > 2)
bg_n <- min(nrow(pts), 20000)
bg  <- if (nrow(pts) > bg_n) pts[sample.int(nrow(pts), bg_n), ] else pts

p_hot <- ggplot() +
  geom_point(data = st_drop_geometry(bg),  aes(longitude, latitude), alpha = 0.08, size = 0.5) +
  geom_point(data = st_drop_geometry(hot), aes(longitude, latitude), alpha = 0.9,  size = 0.6) +
  coord_equal() +
  labs(title = "Hotspots of severe outcomes (Getis–Ord Gi*; z > 2)",
       subtitle = "8-nearest-neighbour weights on projected coordinates (jittered only for neighbors)",
       x = "Longitude", y = "Latitude") +
  theme_minimal(base_size = 11)
ggsave(here("plots","q2_spatial","q2_hotspots_localG.png"), p_hot, width = 7.5, height = 6, dpi = 130)

# -----------------------------
# 4) Grid-level Local Moran’s I on severe rate (no shapefile needed)
# -----------------------------
bb  <- st_as_sfc(st_bbox(pts_3111))
grid <- st_make_grid(bb, cellsize = 20000, what = "polygons", square = TRUE) %>% st_sf()
grid$id <- seq_len(nrow(grid))

# join points -> grid ids
join_idx <- st_within(pts_3111, grid)
pid <- vapply(join_idx, function(x) if (length(x)) x[1] else NA_integer_, integer(1))

grid_stats <- q2[, .(
  crashes_n = .N,
  severe_n  = sum(severe01, na.rm = TRUE),
  severe_rate = mean(severe01, na.rm = TRUE)
), by = pid][!is.na(pid)]

grid <- left_join(grid, grid_stats, by = c("id"="pid")) %>%
  mutate(crashes_n = replace_na(crashes_n, 0),
         severe_rate = replace_na(severe_rate, 0))

# neighbors & LISA
nbg <- spdep::poly2nb(as_Spatial(st_make_valid(grid)))
lwg <- spdep::nb2listw(nbg, style = "W", zero.policy = TRUE)
lm  <- spdep::localmoran(grid$severe_rate, lwg, zero.policy = TRUE)
lm_df <- as.data.frame(lm); names(lm_df) <- c("Ii","E.Ii","Var.Ii","Z.Ii","Pr(z>0)")
grid$lisa_I <- lm_df$Ii; grid$lisa_Z <- lm_df$`Z.Ii`

# export results
grid_out <- grid %>% st_drop_geometry() %>% as.data.frame()
fwrite(grid_out, here("models","q2_local_moran_grid.csv"))

# Choropleth with safe, unique breaks
brks <- safe_breaks(grid$severe_rate, n = 5)
p_grid <- ggplot() +
  geom_sf(data = grid, aes(fill = cut(severe_rate, brks, include.lowest = TRUE)), color = NA) +
  labs(title = "Severe crash rate by 20km grid (quantiles with safe breaks)", fill = "Severe rate") +
  theme_minimal(base_size = 11)
ggsave(here("plots","q2_spatial","q2_grid_severe_rate.png"), p_grid, width = 7.5, height = 6, dpi = 130)

cat("Artifacts written to plots/q2_spatial and models/.\n")
