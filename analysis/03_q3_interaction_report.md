# Q3: How do environmental conditions interact with driver demographics and vehicle type to increase the likelihood of fatal or serious crashes?

This note explains what we modeled for Q3, why we chose these variables, and how to reproduce the results and visuals. It’s concise and focused on interpretation, with light jargon and clear screenshot guidance.

## Data and features

We use the accident-level dataset `dataset_q3_interaction.csv` produced by `R-Q3-Prae/01_clean_feature.R`. It contains:
- Outcome: `is_severe` (1 = Fatal or Serious injury, 0 = Other injury)
- Environmental: `weather_clean` (fine/clear, rain/storm, fog/mist, windy, other), `surface_clean` (dry, wet, loose/other, other), `light_clean` (daylight, dawn, dusk, dark variants)
- Demographics: `age_group_clean` (younger, adult, middle, older, other), `sex_common` (male/female/unknown)
- Vehicle: `vehicle_type_main` (car, truck, motorcycle, etc.), `vehicle_age_avg` (years), plus counts `n_vehicles` and `n_persons`

All categorical variables include an explicit `unknown` level so that missing data doesn’t drop rows.

## Model

We estimate a logistic regression (logit) predicting the probability of a fatal/serious crash, including targeted interaction terms that align to the question:
- Environment × Demographics/Vehicles
  - `weather_clean * age_group_clean`
  - `weather_clean * sex_common`
  - `weather_clean * vehicle_type_main`
  - `light_clean * age_group_clean`
  - `surface_clean * vehicle_type_main`
- Additive controls: `vehicle_age_avg + n_vehicles + n_persons`

The logit is interpretable via odds ratios (OR): OR > 1 increases severe-crash odds relative to a baseline (fine/clear weather, daylight, dry surface, adult, female, car) when those levels exist in the data.

## Key figures to include

- Figure A (Predicted severe risk: Weather × Vehicle type)
  - File: `plots/q3_model/q3_weather_x_vehicle.png`
  - Screenshot caption: “Predicted probability of fatal/serious crash by Weather and Vehicle type (baseline: daylight, dry, adult, male).”
- Figure B (Predicted severe risk: Lighting × Age group)
  - File: `plots/q3_model/q3_light_x_age.png`
  - Screenshot caption: “Predicted probability of fatal/serious crash by Lighting and Driver age group (baseline: fine/clear, dry, car, male).”
- Figure C (Predicted severe risk: Surface × Vehicle type)
  - File: `plots/q3_model/q3_surface_x_vehicle.png`
  - Screenshot caption: “Predicted probability of fatal/serious crash by Road surface and Vehicle type (baseline: fine/clear, daylight, adult, male).”
- Table D (Odds ratios with 95% CI)
  - File: `models/q3_logit_coefficients.csv`
  - Screenshot caption: “Logistic regression odds ratios and 95% CI; interaction terms show condition-specific multipliers.”

## Findings (interpretation template)

- Weather × Vehicle type: Rain/storm typically raises risk across vehicles; motorcycles and heavy vehicles often show higher sensitivity to adverse weather compared to cars.
- Lighting × Age group: Dusk/dawn and dark conditions elevate risk; younger or older drivers may show stronger increases relative to adults during low light.
- Surface × Vehicle type: Wet surfaces elevate risk, especially for motorcycles and lighter vehicles; dry baseline is safest.
- Controls: More vehicles or persons per crash scene can correlate with severity; older vehicle fleets can indicate higher risk if safety technology is outdated.

Replace these with your dataset-specific observations using the saved figures and odds-ratio table.

## How to run

1) Ensure the dataset exists (run once):
- `R-Q3-Prae/01_clean_feature.R` generates `dataset_q3_interaction.csv`.

2) Fit the model and create figures:
- Run `R-Q3-Prae/02_model.R` in RStudio or via terminal using Rscript.
- Artifacts created:
  - `models/q3_logit_coefficients.csv` (odds ratios)
  - `models/q3_logit_metrics.csv` (accuracy, AUC)
  - `models/q3_predicted_grid.csv` (predicted risks across key grids)
  - `plots/q3_model/*.png` (heatmaps for interactions)

## Glossary (light)

- Logistic regression (logit): A model that outputs probabilities between 0 and 1 via a log-odds link; odds ratios > 1 mean higher risk vs baseline.
- Interaction term (A × B): The effect of A depends on B (e.g., rain affects motorcycles more than cars), visible as interaction coefficients and distinct tiles in the heatmaps.
- Odds ratio (OR): Multiplicative change in odds; OR = 1.5 means 50% higher odds; OR < 1 means lower odds.

## Limitations

- Some categories may be sparse; we use compact interactions to keep estimates stable.
- Unknown levels capture missingness; if many rows are unknown, interpret with caution.
- The model is associative, not causal; it identifies amplified risk patterns rather than proving causation.
