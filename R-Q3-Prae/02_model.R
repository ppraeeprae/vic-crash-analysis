# ============================================
# 02_model.R (Q3)
# Modeling interactions: environment x demographics/vehicle -> severe crash risk
# Input  : dataset_q3_interaction.csv
# Output : models/q3_logit_coefficients.csv, models/q3_logit_metrics.csv,
#          models/q3_predicted_grid.csv, plots/q3_model/*.png
# ============================================

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(forcats)
  library(broom)
  library(pROC)
  library(ggplot2)
})

setwd(here::here())
cat("Working dir:", getwd(), "\n")

# 1) Load data -------------------------------------------------------------
path_in <- here("dataset_q3_interaction.csv")
stopifnot(file.exists(path_in))

df <- readr::read_csv(path_in, show_col_types = FALSE)

# 2) Basic cleaning / factor refs -----------------------------------------
# ensure categorical levels have explicit 'unknown' where NA
f_na <- function(x) forcats::fct_na_value_to_level(as.factor(x), level = "unknown")

# set reference levels where sensible for interpretability
lvl_if <- function(x, ref) if (ref %in% levels(x)) relevel(x, ref = ref) else x

# coerce types
cols_cat <- c("weather_clean", "surface_clean", "light_clean",
              "age_group_clean", "sex_common", "vehicle_type_main")
for (nm in cols_cat) {
  if (!nm %in% names(df)) stop(sprintf("Missing expected column: %s", nm))
  df[[nm]] <- f_na(df[[nm]])
}

# numeric buffers and coercion
num_candidates <- c("vehicle_age_avg","n_vehicles","n_persons")
for (nm in setdiff(num_candidates, names(df))) df[[nm]] <- NA_real_
for (nm in num_candidates) df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))

# apply reference levels
df <- df %>% mutate(
  weather_clean    = lvl_if(weather_clean,    "fine/clear"),
  surface_clean    = lvl_if(surface_clean,    "dry"),
  light_clean      = lvl_if(light_clean,      "daylight"),
  age_group_clean  = lvl_if(age_group_clean,  "adult"),
  sex_common       = lvl_if(sex_common,       "female"),
  vehicle_type_main= lvl_if(vehicle_type_main,"car")
)

# response
if (!"is_severe" %in% names(df)) stop("Missing column is_severe")

# drop rows with missing response
df <- df %>% filter(!is.na(is_severe))

# sanity: ensure response has both classes
if (length(unique(df$is_severe)) < 2) {
  stop("Response 'is_severe' has fewer than 2 classes after filtering. Cannot fit model.")
}

# 3) Model specification (robust to single-level and zero-variance vars) ---
has_2lv <- function(f) length(levels(f)) >= 2

# identify categorical predictors with >=2 levels
valid_cat <- cols_cat[vapply(df[cols_cat], has_2lv, logical(1))]
if (length(valid_cat) == 0) warning("All categorical predictors have <2 levels; model reduces to numeric terms only.")

# define candidate interactions
int_pairs <- list(
  c("weather_clean","age_group_clean"),
  c("weather_clean","sex_common"),
  c("weather_clean","vehicle_type_main"),
  c("light_clean","age_group_clean"),
  c("surface_clean","vehicle_type_main")
)

# keep only interactions where both variables are valid
int_terms <- purrr::map_chr(int_pairs, function(p){
  if (all(p %in% valid_cat)) paste0(p[1], "*", p[2]) else NA_character_
})
int_terms <- int_terms[!is.na(int_terms)]

# main effects: include valid categorical variables (dedupe with interactions OK)
main_terms <- valid_cat

# numeric terms: keep only those with variance > 0 and at least one non-NA
has_var <- function(x) {
  x <- as.numeric(x)
  if (all(is.na(x))) return(FALSE)
  v <- stats::var(x, na.rm = TRUE)
  is.finite(v) && v > 0
}
num_terms <- num_candidates[vapply(df[num_candidates], has_var, logical(1))]

all_terms <- c(main_terms, int_terms, num_terms)
all_terms <- unique(all_terms[nchar(all_terms) > 0])

if (length(all_terms) == 0) stop("No valid predictors available to fit the model.")

form <- as.formula(paste("is_severe ~", paste(all_terms, collapse = " + ")))

# brief diagnostic
cat("Using predictors:\n  main:", paste(main_terms, collapse=", "),
    "\n  interactions:", paste(int_terms, collapse=", "),
    "\n  numeric:", paste(num_terms, collapse=", "), "\n")

# 4) Fit logistic regression ----------------------------------------------
fit <- glm(form, data = df, family = binomial())
cat("Model fit complete.\n")

# 5) Coefficients: OR + CI -------------------------------------------------
# Use normal approximation for CI on log-odds for speed
coefs <- broom::tidy(fit, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE)
coefs <- coefs %>% rename(
  term = term,
  odds_ratio = estimate,
  conf_low = conf.low,
  conf_high = conf.high,
  p_value = p.value
)

# 6) Metrics ---------------------------------------------------------------
# predictions
pred_prob <- as.numeric(predict(fit, type = "response"))

# accuracy at 0.5
pred_label <- ifelse(pred_prob >= 0.5, 1, 0)
acc <- mean(pred_label == df$is_severe)

# AUC using pROC
auc_val <- tryCatch({
  as.numeric(pROC::auc(df$is_severe, pred_prob))
}, error = function(e) NA_real_)

metrics <- tibble(
  metric = c("accuracy@0.5", "auc"),
  value = c(acc, auc_val)
)

# 7) Interaction grids and plots ------------------------------------------
dir.create(here("plots","q3_model"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("models"), recursive = TRUE, showWarnings = FALSE)

# helper to predict on grid with sensible defaults for other vars
mean_na_rm <- function(x) if (all(is.na(x))) 0 else mean(x, na.rm = TRUE)

mk_pred <- function(grid) {
  base <- tibble(
    vehicle_age_avg = mean_na_rm(df$vehicle_age_avg),
    n_vehicles = round(mean_na_rm(df$n_vehicles)),
    n_persons  = round(mean_na_rm(df$n_persons))
  )
  g <- cbind(grid, base[rep(1, nrow(grid)), , drop = FALSE])
  g$prob <- as.numeric(predict(fit, newdata = g, type = "response"))
  g
}

# a) weather x vehicle_type_main (fix light=daylight, surface=dry, age=adult, sex=male)
lev <- function(v) levels(df[[v]])

grid_w_v <- expand.grid(
  weather_clean = lev("weather_clean"),
  vehicle_type_main = lev("vehicle_type_main")
) %>% as_tibble() %>% mutate(
  light_clean = factor("daylight", levels = lev("light_clean")),
  surface_clean = factor("dry", levels = lev("surface_clean")),
  age_group_clean = factor("adult", levels = lev("age_group_clean")),
  sex_common = factor(ifelse("male" %in% lev("sex_common"), "male", levels(df$sex_common)[1]),
                      levels = lev("sex_common"))
)

pred_w_v <- mk_pred(grid_w_v)

p_w_v <- ggplot(pred_w_v, aes(x = vehicle_type_main, y = weather_clean, fill = prob)) +
  geom_tile(color = "white") +
  scale_fill_gradient(name = "P(severe)", low = "#f0f9e8", high = "#084081") +
  labs(title = "Predicted severe risk: Weather x Vehicle type",
       x = "Vehicle type", y = "Weather") +
  theme_minimal(base_size = 12)

ggsave(filename = here("plots","q3_model","q3_weather_x_vehicle.png"), p_w_v,
       width = 9, height = 6, dpi = 150)

# b) light x age_group (fix weather=fine/clear, surface=dry, vehicle=car, sex=male)
grid_l_a <- expand.grid(
  light_clean = lev("light_clean"),
  age_group_clean = lev("age_group_clean")
) %>% as_tibble() %>% mutate(
  weather_clean = factor(ifelse("fine/clear" %in% lev("weather_clean"), "fine/clear", levels(df$weather_clean)[1]),
                         levels = lev("weather_clean")),
  surface_clean = factor("dry", levels = lev("surface_clean")),
  vehicle_type_main = factor(ifelse("car" %in% lev("vehicle_type_main"), "car", levels(df$vehicle_type_main)[1]),
                             levels = lev("vehicle_type_main")),
  sex_common = factor(ifelse("male" %in% lev("sex_common"), "male", levels(df$sex_common)[1]),
                      levels = lev("sex_common"))
)

pred_l_a <- mk_pred(grid_l_a)

p_l_a <- ggplot(pred_l_a, aes(x = age_group_clean, y = light_clean, fill = prob)) +
  geom_tile(color = "white") +
  scale_fill_gradient(name = "P(severe)", low = "#f0f9e8", high = "#7b3294") +
  labs(title = "Predicted severe risk: Lighting x Age group",
       x = "Age group", y = "Lighting") +
  theme_minimal(base_size = 12)

ggsave(filename = here("plots","q3_model","q3_light_x_age.png"), p_l_a,
       width = 9, height = 6, dpi = 150)

# c) surface x vehicle_type_main (fix weather=fine/clear, light=daylight, age=adult, sex=male)
grid_s_v <- expand.grid(
  surface_clean = lev("surface_clean"),
  vehicle_type_main = lev("vehicle_type_main")
) %>% as_tibble() %>% mutate(
  weather_clean = factor(ifelse("fine/clear" %in% lev("weather_clean"), "fine/clear", levels(df$weather_clean)[1]),
                         levels = lev("weather_clean")),
  light_clean = factor("daylight", levels = lev("light_clean")),
  age_group_clean = factor("adult", levels = lev("age_group_clean")),
  sex_common = factor(ifelse("male" %in% lev("sex_common"), "male", levels(df$sex_common)[1]),
                      levels = lev("sex_common"))
)

pred_s_v <- mk_pred(grid_s_v)

p_s_v <- ggplot(pred_s_v, aes(x = vehicle_type_main, y = surface_clean, fill = prob)) +
  geom_tile(color = "white") +
  scale_fill_gradient(name = "P(severe)", low = "#f7f7f7", high = "#ca0020") +
  labs(title = "Predicted severe risk: Surface x Vehicle type",
       x = "Vehicle type", y = "Surface") +
  theme_minimal(base_size = 12)

ggsave(filename = here("plots","q3_model","q3_surface_x_vehicle.png"), p_s_v,
       width = 9, height = 6, dpi = 150)

# 8) Save artifacts --------------------------------------------------------
readr::write_csv(coefs, here("models","q3_logit_coefficients.csv"))
readr::write_csv(metrics, here("models","q3_logit_metrics.csv"))

pred_grid <- bind_rows(
  pred_w_v %>% mutate(grid = "weather_x_vehicle"),
  pred_l_a %>% mutate(grid = "light_x_age"),
  pred_s_v %>% mutate(grid = "surface_x_vehicle")
)
readr::write_csv(pred_grid, here("models","q3_predicted_grid.csv"))

cat("Q3 model artifacts saved to models/ and plots/q3_model/.\n")
