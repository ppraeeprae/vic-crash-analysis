## Q1 Severity Modeling (script version)
## - Loads processed features (accident-level preferred)
## - Trains multinomial model; optional ordinal model
## - Saves metrics, confusion matrices, coefficients, importance, and predictions as CSVs

suppressPackageStartupMessages({
	library(tidyverse)
	library(data.table)
	library(here)
	library(nnet)   # multinomial logistic regression
	library(MASS)   # ordinal logistic regression (polr)
	library(forcats)
})

# 1) Load processed data
proc_path_acc <- here("data", "processed", "q1_severity_accident.csv")
proc_path_row <- here("data", "processed", "q1_severity_clean.csv")

if (file.exists(proc_path_acc)) {
	message("Using accident-level dataset: ", proc_path_acc)
	q1 <- fread(proc_path_acc)
} else if (file.exists(proc_path_row)) {
	message("Using row-level dataset: ", proc_path_row)
	q1 <- fread(proc_path_row)
} else {
	stop("No processed dataset found. Run R-Q1-Ploy/01_clean_feature.R first.")
}

# Ensure severity is a factor in expected order
if (!is.factor(q1$severity)) {
	q1[, severity := factor(severity, levels = c("Fatal", "Serious", "Minor"))]
}

cat("Dataset dims:", paste(dim(q1), collapse = " x "), "\n")

# 2) Minimal preprocessing for modeling
candidate_cols <- c(
	"accident_no", "severity", "speed_zone", "light_condition", "weather",
	"road_surface", "road_geometry_desc",
	"main_vehicle_body", "main_vehicle_make",
	"age_group",
	# accident-level features if present
	"vehicles_n", "drivers_n", "driver_pct_male", "driver_age_group_mode"
)

avail <- intersect(candidate_cols, names(q1))
q <- q1[, ..avail]
q[, accident_no := NULL]

# Cast character columns to factor
char_cols <- names(q)[vapply(q, is.character, logical(1))]
for (cc in char_cols) set(q, j = cc, value = factor(q[[cc]]))

# Replace NA in factor columns with "Unknown" (except target)
fac_cols <- names(q)[vapply(q, is.factor, logical(1))]
for (fc in setdiff(fac_cols, "severity")) {
	x <- q[[fc]]
	if (anyNA(x)) {
		x <- fct_explicit_na(x, na_level = "Unknown")
		set(q, j = fc, value = x)
	}
}

# Ensure numeric speed_zone if present
if ("speed_zone" %in% names(q)) set(q, j = "speed_zone", value = as.numeric(q$speed_zone))

# Reduce cardinality for vehicle_make
if ("main_vehicle_make" %in% names(q)) {
	q[, main_vehicle_make := forcats::fct_lump_n(main_vehicle_make, n = 15)]
}

# Drop rows with missing target
q <- q[!is.na(severity)]

cat("Post-preprocess dims:", paste(dim(q), collapse = " x "), "\n")

# 3) Train/test split (stratified by severity)
set.seed(123)
train_idx <- q %>%
	mutate(.row = dplyr::row_number()) %>%
	group_by(severity) %>%
	sample_frac(0.7) %>%
	pull(.row)

train <- q[train_idx]
test  <- q[-train_idx]

cat("Train rows:", nrow(train), " Test rows:", nrow(test), "\n")

# 4) Baseline majority class
maj_class <- names(sort(table(train$severity), decreasing = TRUE))[1]
preds_base <- factor(rep(maj_class, nrow(test)), levels = levels(test$severity))
acc_base <- mean(preds_base == test$severity, na.rm = TRUE)
cat("Baseline majority-class accuracy:", round(acc_base, 4), "\n")

# 5) Multinomial logistic regression
pred_cols <- setdiff(names(train), "severity")
form <- as.formula(paste("severity ~", paste(pred_cols, collapse = " + ")))

max_rows <- 150000L
if (nrow(train) > max_rows) {
	set.seed(123)
	train_fit <- train %>%
		mutate(.n = dplyr::row_number()) %>%
		group_by(severity) %>%
		sample_n(ceiling(max_rows * n() / nrow(train))) %>%
		ungroup() %>%
		as.data.frame()
	cat("Using stratified downsample for training:", nrow(train_fit), "rows\n")
} else {
	train_fit <- as.data.frame(train)
}

model_multinom <- nnet::multinom(form, data = train_fit, trace = FALSE, MaxNWts = 20000, maxit = 200)

preds <- predict(model_multinom, newdata = as.data.frame(test), type = "class")
acc <- mean(preds == test$severity, na.rm = TRUE)
cat("Multinomial accuracy:", round(acc, 4), "\n")
mn_cm <- as.matrix(table(Actual = test$severity, Pred = preds))

# 5b) Variable importance from coefficients
coefs <- summary(model_multinom)$coefficients
if (is.list(coefs)) {
	coef_mat <- do.call(rbind, coefs)
} else {
	coef_mat <- coefs
}
imp <- tibble(
	feature = colnames(coef_mat),
	importance = colSums(abs(coef_mat))
) %>% arrange(desc(importance)) %>% slice(1:20)

print(imp)

# 6) Optional: Ordinal logistic regression (guarded)
sev_ord_levels <- c("Minor", "Serious", "Fatal")
q_ord <- copy(q)
q_ord[, severity := factor(as.character(severity), levels = sev_ord_levels, ordered = TRUE)]
train_o <- q_ord[train_idx]
test_o  <- q_ord[-train_idx]
form_o <- as.formula(paste("severity ~", paste(setdiff(names(train_o), "severity"), collapse = " + ")))

polr_ok <- FALSE
model_polr <- NULL
preds_o <- NULL
acc_o <- NA_real_

try({
	model_polr <- MASS::polr(form_o, data = as.data.frame(train_o), Hess = TRUE)
	preds_o <- predict(model_polr, newdata = as.data.frame(test_o), type = "class")
	acc_o <- mean(preds_o == test_o$severity)
	polr_ok <- TRUE
}, silent = TRUE)

if (isTRUE(polr_ok)) {
	cat("Ordinal (polr) accuracy:", round(acc_o, 4), "\n")
	print(table(Actual = test_o$severity, Pred = preds_o))
} else {
	message("Skipping ordinal outputs due to fitting/prediction issue (e.g., rank deficiency).")
}

# 6b) Visualizations (shown in RStudio Plots pane and optionally saved)
plots_dir <- here("plots", "q1_model")
if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)

# Accuracy comparison bar chart
metrics_df <- tibble(
	model = c("Baseline", "Multinomial"),
	accuracy = c(acc_base, acc)
)
if (isTRUE(polr_ok)) metrics_df <- bind_rows(metrics_df, tibble(model = "Ordinal", accuracy = acc_o))

p_acc <- ggplot(metrics_df, aes(x = model, y = accuracy, fill = model)) +
	geom_col(width = 0.6, show.legend = FALSE) +
	geom_text(aes(label = sprintf("%0.1f%%", accuracy * 100)), vjust = -0.4, size = 3) +
	scale_y_continuous(limits = c(0, 1)) +
	labs(title = "Severity model accuracy by approach", x = NULL, y = "Accuracy (proportion)") +
	theme_minimal(base_size = 11) +
	theme(plot.title = element_text(face = "bold"))
print(p_acc)
try(suppressWarnings(ggsave(filename = here(plots_dir, "q1_accuracy.png"), plot = p_acc, width = 7, height = 4, dpi = 120)), silent = TRUE)

# Confusion matrix heatmap (Multinomial)
cm_df <- as.data.frame(as.table(mn_cm)) %>%
	as_tibble()
# Standardize column names regardless of whether as.data.frame() produced Var1/Var2 or Actual/Pred
if ("Var1" %in% names(cm_df)) cm_df <- cm_df %>% rename(Actual = Var1)
if ("Var2" %in% names(cm_df)) cm_df <- cm_df %>% rename(Pred = Var2)
if ("Freq" %in% names(cm_df)) cm_df <- cm_df %>% rename(n = Freq)
cm_df <- cm_df %>%
	group_by(Actual) %>%
	mutate(pct = ifelse(sum(n) > 0, n / sum(n), 0)) %>%
	ungroup()

p_cm <- ggplot(cm_df, aes(x = Pred, y = Actual, fill = pct)) +
	geom_tile(color = "white") +
	geom_text(aes(label = n), color = "white", fontface = "bold", size = 3) +
	scale_fill_gradient(low = "#dce9f2", high = "#2b6cb0", limits = c(0, 1)) +
	coord_equal() +
	labs(title = "Confusion matrix (Multinomial)", x = "Predicted", y = "Actual", fill = "Row %") +
	theme_minimal(base_size = 11) +
	theme(plot.title = element_text(face = "bold"))
print(p_cm)
try(suppressWarnings(ggsave(filename = here(plots_dir, "q1_confusion_multinomial.png"), plot = p_cm, width = 6, height = 5, dpi = 120)), silent = TRUE)

# Variable importance bar plot (Multinomial)
if (exists("imp") && is.data.frame(imp) && nrow(imp) > 0) {
	imp_plot <- imp %>%
		mutate(feature = forcats::fct_reorder(feature, importance)) %>%
		ggplot(aes(x = importance, y = feature)) +
		geom_col(fill = "#3182bd") +
		labs(title = "Top 20 features by coefficient magnitude (Multinomial)", x = "Importance (|coef| sum)", y = NULL) +
		theme_minimal(base_size = 11) +
		theme(plot.title = element_text(face = "bold"))
	print(imp_plot)
	try(suppressWarnings(ggsave(filename = here(plots_dir, "q1_importance_top20.png"), plot = imp_plot, width = 7, height = 6, dpi = 120)), silent = TRUE)
}

# Optional: Confusion matrix heatmap (Ordinal) if available
if (isTRUE(polr_ok)) {
	plr_cm <- as.matrix(table(Actual = test_o$severity, Pred = preds_o))
	cm_o_df <- as.data.frame(as.table(plr_cm)) %>%
		as_tibble()
	if ("Var1" %in% names(cm_o_df)) cm_o_df <- cm_o_df %>% rename(Actual = Var1)
	if ("Var2" %in% names(cm_o_df)) cm_o_df <- cm_o_df %>% rename(Pred = Var2)
	if ("Freq" %in% names(cm_o_df)) cm_o_df <- cm_o_df %>% rename(n = Freq)
	cm_o_df <- cm_o_df %>%
		group_by(Actual) %>%
		mutate(pct = ifelse(sum(n) > 0, n / sum(n), 0)) %>%
		ungroup()
	p_cm_o <- ggplot(cm_o_df, aes(x = Pred, y = Actual, fill = pct)) +
		geom_tile(color = "white") +
		geom_text(aes(label = n), color = "white", fontface = "bold", size = 3) +
		scale_fill_gradient(low = "#e9d8fd", high = "#6b46c1", limits = c(0, 1)) +
		coord_equal() +
		labs(title = "Confusion matrix (Ordinal polr)", x = "Predicted", y = "Actual", fill = "Row %") +
		theme_minimal(base_size = 11) +
		theme(plot.title = element_text(face = "bold"))
	print(p_cm_o)
	try(suppressWarnings(ggsave(filename = here(plots_dir, "q1_confusion_ordinal.png"), plot = p_cm_o, width = 6, height = 5, dpi = 120)), silent = TRUE)
}

# 7) Save artifacts as CSVs
models_dir <- here("models")
if (!dir.exists(models_dir)) dir.create(models_dir, recursive = TRUE)

# Multinomial
mn_coefs <- summary(model_multinom)$coefficients
if (is.list(mn_coefs)) {
	mn_coef_mat <- do.call(rbind, mn_coefs)
} else {
	mn_coef_mat <- mn_coefs
}
write.csv(mn_coef_mat, here("models", "q1_multinom_coefficients.csv"), row.names = TRUE)
write.csv(mn_cm,      here("models", "q1_multinom_confusion.csv"),   row.names = TRUE)
write.csv(data.frame(
	baseline_accuracy = acc_base,
	multinom_accuracy = acc
), here("models", "q1_multinom_metrics.csv"), row.names = FALSE)

# Test predictions with class probabilities
mn_probs <- tryCatch(predict(model_multinom, newdata = as.data.frame(test), type = "probs"), error = function(e) NULL)
if (!is.null(mn_probs)) {
	preds_df <- cbind(
		data.frame(Actual = test$severity, Pred = preds),
		as.data.frame(mn_probs)
	)
	write.csv(preds_df, here("models", "q1_multinom_test_predictions.csv"), row.names = FALSE)
}

if (exists("imp")) write.csv(imp, here("models", "q1_multinom_importance_top20.csv"), row.names = FALSE)
write.csv(data.frame(feature = pred_cols), here("models", "q1_multinom_features.csv"), row.names = FALSE)
write.csv(data.frame(severity_levels = levels(q$severity)), here("models", "q1_severity_levels.csv"), row.names = FALSE)

# Ordinal (only if successful)
if (isTRUE(polr_ok)) {
	plr_coefs <- coef(summary(model_polr))
	write.csv(plr_coefs, here("models", "q1_polr_coefficients.csv"))
	plr_cm <- as.matrix(table(Actual = test_o$severity, Pred = preds_o))
	write.csv(plr_cm, here("models", "q1_polr_confusion.csv"), row.names = TRUE)
	write.csv(data.frame(ordinal_accuracy = acc_o), here("models", "q1_polr_metrics.csv"), row.names = FALSE)
}

cat("Saved model artifacts to:", models_dir, "\n")
