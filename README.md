# Victorian Crash Severity Prediction and Hotspot Mapping

This project analyzes Victorian road crash data (9 joined tables) to predict crash severity and identify spatial hotspots using R.

## 📁 Folder Structure
| Folder | Description |
|--------|--------------|
| **/data** | Raw CSV datasets (Accident, Vehicle, Person, Road Surface, etc.) |
| **/R** | R scripts for data loading, cleaning, modeling, and spatial analysis |
| **/figures** | Charts, model evaluation plots, and hotspot maps |
| **/reports** | Summary outputs, markdown reports, and dashboards |

## ⚙️ Tools & Libraries
- **R version:** 4.5.1  
- **Key packages:** `tidyverse`, `dplyr`, `lubridate`, `caret`, `randomForest`, `ROSE`, `pROC`, `sf`, `leaflet`

## 🧭 Workflow
1. Load and join 9 crash-related datasets by `accident_no`
2. Clean and engineer key features (e.g. Driver age, Crash time, Weather × Road index)
3. Train models (Logistic Regression, Random Forest) to predict severity
4. Evaluate using stratified cross-validation (Macro-F1, Recall of fatal class)
5. Visualize spatial clusters using `sf` and `leaflet`
6. Publish dashboard summarizing key insights

## 🧩 Authors
Project maintained by **Ruethaimart Kongchuensin** (Master of IT, Swinburne University)

---

