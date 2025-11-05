# ============================================================
# 02_model.R
# Purpose: Model for Guiding Question 3
# ------------------------------------------------------------
# "How do weather, lighting, and road-surface conditions
#  interact with driver demographics and vehicle type
#  to increase the likelihood of fatal or serious crashes?"
# ============================================================

library(tidyverse)
library(janitor)
library(broom)
library(forcats)

# ------------------------------------------------------------
# 1. Load driver-level dataset (created in 01_clean_feature.R)
# ------------------------------------------------------------
q3_path   <- file.path("data", "clean", "q3_driver_environment.csv")
full_path <- file.path("data", "clean", "merged_with_features.csv")

if (!file.exists(q3_path)) {
  stop("❌ Cannot find q3_driver_environment.csv. Please run 01_clean_feature.R first.")
}

q3 <- read_csv(q3_path, show_col_types = FALSE) |> clean_names()
n_q3 <- nrow(q3)
message("📄 q3_driver_environment.csv rows: ", n_q3)

# ------------------------------------------------------------
# 2. If Q3 dataset is empty, rebuild it from merged_with_features.csv
# ------------------------------------------------------------
if (n_q3 == 0) {
  message("⚠️ Q3 dataset is empty. Trying to rebuild from merged_with_features.csv ...")
  
  if (!file.exists(full_path)) stop("❌ No fallback file found at ", full_path)
  
  df_full <- read_csv(full_path, show_col_types = FALSE) |> clean_names()
  
  # Create target variable
  df_full <- df_full |>
    mutate(
      severity_flag = case_when(
        severity %in% c(1, 2) ~ 1L,
        severity %in% c(3, 4) ~ 0L,
        TRUE ~ NA_integer_
      )
    )
  
  # Keep only records with both vehicle and person info
  q3 <- df_full |>
    filter(!is.na(severity_flag), !is.na(vehicle_id) | !is.na(vehicle_type_desc))
  
  # Create missing environmental groupings if necessary
  if (!"weather_group" %in% names(q3)) {
    q3 <- q3 |>
      mutate(
        weather_group = case_when(
          str_detect(str_to_lower(atmosph_cond_desc %||% ""), "rain") ~ "Rain",
          str_detect(str_to_lower(atmosph_cond_desc %||% ""), "fog")  ~ "Fog/Smoke",
          str_detect(str_to_lower(atmosph_cond_desc %||% ""), "wind") ~ "Windy",
          str_detect(str_to_lower(atmosph_cond_desc %||% ""), "clear") ~ "Clear",
          !is.na(atmosph_cond_desc) ~ "Other",
          TRUE ~ NA_character_
        )
      )
  }
  
  if (!"surface_group" %in% names(q3)) {
    q3 <- q3 |>
      mutate(
        surface_group = case_when(
          str_detect(str_to_lower(surface_cond_desc %||% ""), "dry")   ~ "Dry",
          str_detect(str_to_lower(surface_cond_desc %||% ""), "wet")   ~ "Wet",
          str_detect(str_to_lower(surface_cond_desc %||% ""), "snow")  ~ "Snow/Ice",
          str_detect(str_to_lower(surface_cond_desc %||% ""), "loose") ~ "Loose/Gravel",
          !is.na(surface_cond_desc) ~ "Other",
          TRUE ~ NA_character_
        )
      )
  }
  
  if (!"light_group" %in% names(q3)) {
    q3 <- q3 |>
      mutate(
        light_condition_desc = case_when(
          light_condition %in% c(1)              ~ "Daylight",
          light_condition %in% c(2, 3)           ~ "Dusk/Dawn",
          light_condition %in% c(4, 5, 6, 7, 8)  ~ "Dark",
          !is.na(light_condition)                ~ "Other",
          TRUE                                   ~ NA_character_
        ),
        light_group = case_when(
          str_detect(str_to_lower(light_condition_desc %||% ""), "day")        ~ "Daylight",
          str_detect(str_to_lower(light_condition_desc %||% ""), "dusk|dawn")  ~ "Dusk/Dawn",
          str_detect(str_to_lower(light_condition_desc %||% ""), "dark")       ~ "Dark",
          !is.na(light_condition_desc)                                         ~ "Other",
          TRUE                                                                 ~ NA_character_
        )
      )
  }
  
  n_q3 <- nrow(q3)
  message("✅ Rebuilt Q3-like dataset. Rows: ", n_q3)
}

if (n_q3 == 0) stop("❌ Still no usable data. Check driver filtering in 01_clean_feature.R.")

# ------------------------------------------------------------
# 3. Summarize severity distribution
# ------------------------------------------------------------
severity_summary <- q3 |>
  count(severity_flag) |>
  mutate(percentage = round(100 * n / sum(n), 2))
print(severity_summary)

if (nrow(severity_summary) < 2)
  stop("❌ Only one severity class found (all 0 or all 1). Cannot fit logistic regression.")

# ------------------------------------------------------------
# 4. Prepare modeling dataset
# ------------------------------------------------------------
model_data <- q3 |>
  select(
    severity_flag,
    weather_group, surface_group, light_group,
    sex, age_group,
    vehicle_type_desc, vehicle_body_style,
    time_of_day, is_weekend, speed_zone
  ) |>
  drop_na(severity_flag)

# Convert categorical variables to factors
factor_vars <- intersect(
  c("weather_group", "surface_group", "light_group",
    "sex", "age_group", "vehicle_type_desc",
    "vehicle_body_style", "time_of_day"),
  names(model_data)
)

model_data <- model_data |>
  mutate(across(all_of(factor_vars), as.factor))

# Remove predictors with only 1 level
single_level <- factor_vars[vapply(model_data[factor_vars], function(x) nlevels(x) < 2, logical(1))]
if (length(single_level) > 0) {
  message("⚠️ Dropping predictors with only 1 level: ", paste(single_level, collapse = ", "))
  model_data <- model_data |> select(-all_of(single_level))
}

# Lump rare categories (reduce quasi-separation)
model_data <- model_data %>%
  mutate(
    weather_group = fct_relevel(weather_group, "Clear"),
    surface_group = fct_relevel(surface_group, "Dry"),
    light_group   = fct_relevel(light_group, "Daylight"),
    time_of_day   = fct_relevel(time_of_day, "Afternoon"),
    vehicle_type_desc  = fct_lump_min(vehicle_type_desc,  min = 5000, other_level = "Other vehicle"),
    vehicle_body_style = fct_lump_min(vehicle_body_style, min = 5000, other_level = "Other body"),
    age_group          = fct_lump_min(age_group,          min = 5000, other_level = "Other age")
  )

# ------------------------------------------------------------
# 5. Fit logistic regression model
# ------------------------------------------------------------
predictors <- setdiff(names(model_data), "severity_flag")
formula_str <- paste("severity_flag ~", paste(predictors, collapse = " + "))
form <- as.formula(formula_str)

model <- glm(form, data = model_data, family = binomial(link = "logit"))
cat("\n✅ Logistic regression fitted successfully.\n")

# ------------------------------------------------------------
# 6. Calculate odds ratios (using Wald CIs)
# ------------------------------------------------------------
odds <- tidy(model, exponentiate = TRUE, conf.int = TRUE, conf.method = "wald") |>
  arrange(desc(estimate))

# ------------------------------------------------------------
# 7. Print short summary (no console spam)
# ------------------------------------------------------------
options(max.print = 200)
cat("\nModel AIC:", AIC(model), "\n")
cat("Rows used:", nrow(model_data), "\n\n")
cat("Top 20 predictors by Odds Ratio:\n")
print(odds |> head(20))

# ------------------------------------------------------------
# 8. Save all outputs
# ------------------------------------------------------------
dir.create("data/models", showWarnings = FALSE, recursive = TRUE)

# Save model
saveRDS(model, file.path("data", "models", "q3_logistic_model.rds"))
# Save odds ratio table
write_csv(odds, file.path("data", "models", "q3_logistic_odds_ratios.csv"))

# Save full summary to text file
sink(file.path("data", "models", "q3_logistic_model_summary.txt"))
print(summary(model))
sink()

cat("\n📁 Files saved:\n")
cat("- data/models/q3_logistic_model.rds\n")
cat("- data/models/q3_logistic_odds_ratios.csv\n")
cat("- data/models/q3_logistic_model_summary.txt\n")

# ------------------------------------------------------------
# 9. Optional quick plot (runs only in interactive mode)
# ------------------------------------------------------------
if (interactive()) {
  library(ggplot2)
  p <- ggplot(model_data, aes(x = weather_group, fill = as.factor(severity_flag))) +
    geom_bar(position = "fill") +
    labs(title = "Proportion of Fatal/Serious Crashes by Weather",
         x = "Weather Condition", y = "Proportion") +
    theme_minimal()
  print(p)
}

# Close open graphic devices
while (!is.null(dev.list())) grDevices::dev.off()

cat("\n✅ Done.\n")
