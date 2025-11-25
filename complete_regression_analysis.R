# STA 4164 - Statistical Method III
# Project 2 - Final Regression Analysis
# Student Performance Factors Analysis
# Using Log Transformation with Standard Linear Regression

# =============================================================================
# ENVIRONMENT & SETUP
# =============================================================================

# Load required libraries
library(tidyverse)    # Data manipulation and visualization
library(car)          # Diagnostic plots and VIF
library(MASS)         # Stepwise regression (stepAIC)
library(gridExtra)    # Arrange plots
library(lmtest)       # Durbin-Watson and Breusch-Pagan tests
library(sandwich)     # Robust Standard Errors (HC3)

# Load rstudioapi if available (for automatic working directory detection)
if (requireNamespace("rstudioapi", quietly = TRUE)) {
  library(rstudioapi)
}

# Set seed for reproducibility
set.seed(4164)

# =============================================================================
# 1. DATA LOADING & INITIAL EXPLORATION (UNCHANGED)
# =============================================================================

# Automatically set working directory to script's location
# (Standard setup code here, omitted for brevity)
if (exists("rstudioapi") && rstudioapi::isAvailable()) {
  script_path <- rstudioapi::getActiveDocumentContext()$path
  if (script_path != "") {
    setwd(dirname(script_path))
    cat("Working directory set to:", getwd(), "\n")
  }
} else {
  tryCatch({
    if (requireNamespace("rstudioapi", quietly = TRUE)) {
      script_path <- rstudioapi::getActiveDocumentContext()$path
      if (script_path != "") {
        setwd(dirname(script_path))
        cat("Working directory set to:", getwd(), "\n")
      }
    }
  }, error = function(e) {
    cmd_args <- commandArgs(trailingOnly = FALSE)
    file_arg <- "--file="
    script_path <- sub(file_arg, "", cmd_args[grep(file_arg, cmd_args)])
    if (length(script_path) > 0 && script_path != "") {
      setwd(dirname(normalizePath(script_path)))
      cat("Working directory set to:", getwd(), "\n")
    }
  })
}

# Load the data
cat("\nCurrent working directory:", getwd(), "\n")
cat("Loading StudentPerformanceFactors.csv...\n")

csv_file <- "StudentPerformanceFactors.csv"
if (!file.exists(csv_file)) {
  csv_file <- file.path("C:/Users/ryanm/OneDrive/Documentos/statsproject", "StudentPerformanceFactors.csv")
  if (file.exists(csv_file)) {
    cat("Found CSV file at:", csv_file, "\n")
  } else {
    stop("ERROR: StudentPerformanceFactors.csv not found!\n",
         "Current directory: ", getwd(), "\n",
         "Please ensure the CSV file is in the same directory as this script.")
  }
}

df <- read.csv(csv_file)
if (nrow(df) == 0) {
  stop("ERROR: CSV file loaded but contains 0 rows. Please check the file.")
}
cat("Data loaded successfully! Rows:", nrow(df), ", Columns:", ncol(df), "\n")

# Check for missing values
cat("\nMissing values:\n")
print(sapply(df, function(x) sum(is.na(x))))

# =============================================================================
# 2. DATA PREPARATION (UNCHANGED)
# =============================================================================

cat("\n========== DATA PREPARATION ==========\n")

# Remove rows with missing Exam_Score (outcome variable)
data_clean <- df %>% filter(!is.na(Exam_Score))
cat("\nOriginal rows:", nrow(df), "\n")
cat("After removing missing Exam_Score:", nrow(data_clean), "\n")

# Create dummy variables for categorical predictors
cat("\n--- Creating Dummy Variables ---\n")
data_model <- data_clean %>%
  mutate(
    # Parental Involvement (reference: Low)
    Parental_Medium = as.numeric(Parental_Involvement == "Medium"),
    Parental_High = as.numeric(Parental_Involvement == "High"),
    
    # Access to Resources (reference: Low)
    Access_Medium = as.numeric(Access_to_Resources == "Medium"),
    Access_High = as.numeric(Access_to_Resources == "High"),
    
    # Extracurricular Activities (reference: No)
    Extra_Yes = as.numeric(Extracurricular_Activities == "Yes"),
    
    # Internet Access (reference: No)
    Internet_Yes = as.numeric(Internet_Access == "Yes"),
    
    # Family Income (reference: Low)
    Income_Medium = as.numeric(Family_Income == "Medium"),
    Income_High = as.numeric(Family_Income == "High"),
    
    # Teacher Quality (reference: Low)
    Teacher_Medium = as.numeric(Teacher_Quality == "Medium"),
    Teacher_High = as.numeric(Teacher_Quality == "High"),
    
    # School Type (reference: Public)
    School_Private = as.numeric(School_Type == "Private"),
    
    # Peer Influence (reference: Neutral)
    Peer_Positive = as.numeric(Peer_Influence == "Positive"),
    Peer_Negative = as.numeric(Peer_Influence == "Negative"),
    
    # Learning Disabilities (reference: No)
    Learning_Yes = as.numeric(Learning_Disabilities == "Yes"),
    
    # Parental Education Level (reference: High School)
    ParentEdu_College = as.numeric(Parental_Education_Level == "College"),
    ParentEdu_Postgrad = as.numeric(Parental_Education_Level == "Postgraduate"),
    
    # Distance from Home (reference: Close)
    Distance_Moderate = as.numeric(Distance_from_Home == "Moderate"),
    Distance_Far = as.numeric(Distance_from_Home == "Far"),
    
    # Gender (reference: Female)
    Gender_Male = as.numeric(Gender == "Male"),
    
    # Motivation Level (reference: Low)
    Motivation_Medium = as.numeric(Motivation_Level == "Medium"),
    Motivation_High = as.numeric(Motivation_Level == "High")
  )

cat("Dummy variables created successfully.\n")

# Prepare final modeling dataset (complete cases only)
model_vars <- data_model %>%
  dplyr::select(Exam_Score, 
         Hours_Studied, Attendance, Sleep_Hours, Previous_Scores,
         Tutoring_Sessions, Physical_Activity,
         Parental_Medium, Parental_High, Access_Medium, Access_High,
         Extra_Yes, Internet_Yes, Income_Medium, Income_High,
         Teacher_Medium, Teacher_High, School_Private,
         Peer_Positive, Peer_Negative, Learning_Yes,
         ParentEdu_College, ParentEdu_Postgrad,
         Distance_Moderate, Distance_Far, Gender_Male,
         Motivation_Medium, Motivation_High) %>%
  na.omit()

cat("\nFinal modeling dataset:\n")
cat("Number of observations:", nrow(model_vars), "\n")
cat("Number of predictors:", ncol(model_vars) - 1, "\n")

# =============================================================================
# 3. APPLY LOG TRANSFORMATION
# =============================================================================

cat("\n========== APPLYING LOG TRANSFORMATION ==========\n")
cat("Transforming Exam_Score using log transformation for better normality.\n")
cat("This allows us to use linear regression with real R-squared.\n\n")

# Apply log transformation (shift to avoid log(0))
shift_value <- 1 - min(model_vars$Exam_Score)
model_vars$Exam_Score_Transformed <- log(model_vars$Exam_Score + shift_value)
transformation_type <- "log"
transformation_formula <- paste0("log(Exam_Score + ", shift_value, ")")

cat("Shift value applied:", shift_value, "\n")
cat("Transformation applied:", transformation_formula, "\n")
cat("Original Exam_Score range:", min(model_vars$Exam_Score), "-", max(model_vars$Exam_Score), "\n")
cat("Log-transformed range:", round(min(model_vars$Exam_Score_Transformed), 3), "-", 
    round(max(model_vars$Exam_Score_Transformed), 3), "\n")

# =============================================================================
# 4. STEPWISE MODEL SELECTION
# =============================================================================

cat("\n========== STEPWISE MODEL SELECTION ==========\n")

# Get predictor names (excluding original and transformed score)
pred_vars_all <- names(model_vars)[!names(model_vars) %in% c("Exam_Score", "Exam_Score_Transformed")]

# Full model with all predictors
formula_full <- as.formula(paste("Exam_Score_Transformed ~", paste(pred_vars_all, collapse = " + ")))
full_model <- lm(formula_full, data = model_vars)

# Stepwise selection
cat("\n--- Running Stepwise Selection (AIC) ---\n")
null_model <- lm(Exam_Score_Transformed ~ 1, data = model_vars)
best_model <- stepAIC(null_model, 
                      direction = "both",
                      scope = list(upper = full_model, lower = null_model),
                      trace = 0)

initial_summary <- summary(best_model)
cat("Initial Model R-squared:", round(initial_summary$r.squared, 4), "\n")
cat("Initial Model Adj. R-squared:", round(initial_summary$adj.r.squared, 4), "\n")

# Store formula and selected predictors
model_formula <- formula(best_model)
pred_vars_selected <- names(coef(best_model))[-1]

# =============================================================================
# 5. DIAGNOSTICS FOR ASSUMPTIONS (Base Model)
# =============================================================================

cat("\n========== BASE MODEL ASSUMPTION CHECKS ==========\n")
bp_base <- bptest(best_model)
ks_base <- ks.test(sample(residuals(best_model), 5000), "pnorm", 
                   mean = mean(residuals(best_model)), sd = sd(residuals(best_model)))
dw_base <- durbinWatsonTest(best_model)

cat("Homoscedasticity (Breusch-Pagan p-value):", format(bp_base$p.value, scientific = TRUE), "\n")
cat("Normality (KS test p-value):", format(ks_base$p.value, scientific = TRUE), "\n")
cat("Independence (Durbin-Watson p-value):", format(dw_base$p, scientific = TRUE), "\n")

# =============================================================================
# 6. FINAL MODEL SELECTION
# =============================================================================

cat("\n========== FINAL MODEL SELECTION ==========\n")
cat("Using standard linear regression model.\n\n")

# Use standard model as final model
final_model <- best_model
final_summary <- initial_summary
model_type <- paste("Standard Linear Regression (", transformation_type, ")") 

# =============================================================================
# 7. ROBUST STANDARD ERRORS
# =============================================================================

cat("\n========== COMPUTING ROBUST STANDARD ERRORS ==========\n")
cat("Using standard model with robust standard errors for valid inference.\n\n")

# Compute Robust Standard Errors (HC3 type) for the standard model
robust_se <- sqrt(diag(vcovHC(final_model, type = "HC3")))
robust_summary <- final_summary

# Replace standard errors and recalculate t/p-values
robust_summary$coefficients[, 2] <- robust_se
robust_summary$coefficients[, 3] <- robust_summary$coefficients[, 1] / robust_se
robust_summary$coefficients[, 4] <- 2 * pt(abs(robust_summary$coefficients[, 3]), 
                                          df = final_summary$df[2], lower.tail = FALSE)
final_summary_robust <- robust_summary
cat("Robust standard errors (HC3) computed for valid inference.\n")

cat("\nFinal Model R-squared:", round(final_summary$r.squared, 4), "\n")
cat("Final Model Adjusted R-squared:", round(final_summary$adj.r.squared, 4), "\n")

# =============================================================================
# 8. DIAGNOSTICS & PLOTS (UNCHANGED LOGIC)
# =============================================================================

cat("\n========== MODEL DIAGNOSTICS & PLOTS ==========\n")

residuals_raw <- residuals(final_model)
fitted_values <- fitted(final_model)
residuals_stud <- rstandard(final_model)
cooks_d <- cooks.distance(final_model)
influential <- which(cooks_d > 4 / nrow(model_vars))

# VIF for multicollinearity (already in Section 7.1 of original, moved here for flow)
cat("\n--- Variance Inflation Factors ---\n")
vif_values <- vif(final_model)
high_vif <- vif_values[vif_values > 10]
if(length(high_vif) > 0) {
  cat("Variables with VIF > 10:", paste(names(high_vif), collapse = ", "), "\n")
} else {
  cat("No multicollinearity concerns (all VIF < 10).\n")
}

# ----------------- PLOT GENERATION -----------------
# 1. Q-Q Plot
png("qq_plot_residuals.png", width = 800, height = 600, res = 150)
qq_plot <- ggplot(data.frame(Residuals = residuals_stud), aes(sample = Residuals)) +
  stat_qq(alpha = 0.7, color = "black", size = 0.8) + stat_qq_line(color = "black", linewidth = 1, linetype = "dashed") +
  labs(title = "Q-Q Plot of Studentized Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_bw()
print(qq_plot)
dev.off()
cat("Q-Q plot saved as 'qq_plot_residuals.png'\n")

# 2. Residuals vs Fitted Plot (Linearity Check)
png("residuals_vs_fitted.png", width = 800, height = 600, res = 150)
resid_fitted_df <- data.frame(Fitted = fitted_values, Residuals = residuals_raw)
resid_fitted_plot <- ggplot(resid_fitted_df, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.6, color = "black", size = 0.8) + geom_hline(yintercept = 0, color = "black", linewidth = 1, linetype = "dashed") +
  geom_smooth(method = "loess", se = TRUE, color = "black", linewidth = 1, linetype = "dotted") +
  labs(title = "Residuals vs Fitted Values (Linearity Check)", x = "Fitted Values", y = "Residuals") + theme_bw()
print(resid_fitted_plot)
dev.off()
cat("Residuals vs Fitted plot saved as 'residuals_vs_fitted.png'\n")

# 3. Scale-Location Plot (Homoscedasticity Check)
png("scale_location_plot.png", width = 800, height = 600, res = 150)
sqrt_abs_resid <- sqrt(abs(residuals_stud))
scale_loc_df <- data.frame(Fitted = fitted_values, Sqrt_Abs_Residuals = sqrt_abs_resid)
scale_loc_plot <- ggplot(scale_loc_df, aes(x = Fitted, y = Sqrt_Abs_Residuals)) +
  geom_point(alpha = 0.6, color = "black", size = 0.8) + geom_smooth(method = "loess", se = TRUE, color = "black", linewidth = 1, linetype = "dotted") +
  labs(title = "Scale-Location Plot (Homoscedasticity Check)", x = "Fitted Values", y = "sqrt(|Standardized Residuals|)") + theme_bw()
print(scale_loc_plot)
dev.off()
cat("Scale-Location plot saved as 'scale_location_plot.png'\n")

# 4. Cook's Distance Plot
png("cooks_distance_plot.png", width = 1000, height = 600, res = 150)
cooks_threshold <- 4 / nrow(model_vars)
cooks_df <- data.frame(Observation = seq_along(cooks_d), Cooks_Distance = cooks_d)
cooks_plot <- ggplot(cooks_df, aes(x = Observation, y = Cooks_Distance)) +
  geom_point(alpha = 0.6, color = "black", size = 1.2) + geom_hline(yintercept = cooks_threshold, color = "black", linewidth = 1, linetype = "dashed") +
  labs(title = "Cook's Distance Plot (Influence Check)", subtitle = paste("Threshold: 4/n =", round(cooks_threshold, 6)), x = "Observation Number", y = "Cook's Distance") + theme_bw()
print(cooks_plot)
dev.off()
cat("Cook's distance plot saved as 'cooks_distance_plot.png'\n")

# 5. Residuals Histogram (Normality Check)
png("residuals_histogram.png", width = 800, height = 600, res = 150)
resid_hist_plot <- ggplot(data.frame(Residuals = residuals_raw), aes(x = Residuals)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "gray80", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(residuals_raw), sd = sd(residuals_raw)), color = "black", linewidth = 1, linetype = "dashed") +
  labs(title = "Histogram of Residuals (Normality Check)", x = "Residuals", y = "Density") + theme_bw()
print(resid_hist_plot)
dev.off()
cat("Residuals histogram saved as 'residuals_histogram.png'\n")

# 6. Residuals vs Order Plot (Independence Check)
png("residuals_vs_order.png", width = 800, height = 600, res = 150)
resid_order_df <- data.frame(Order = seq_along(residuals_raw), Residuals = residuals_raw)
resid_order_plot <- ggplot(resid_order_df, aes(x = Order, y = Residuals)) +
  geom_point(alpha = 0.6, color = "black", size = 0.8) + geom_hline(yintercept = 0, color = "black", linewidth = 1, linetype = "dashed") +
  geom_smooth(method = "loess", se = TRUE, color = "black", linewidth = 1, linetype = "dotted") +
  labs(title = "Residuals vs Order (Independence Check)", x = "Observation Order", y = "Residuals") + theme_bw()
print(resid_order_plot)
dev.off()
cat("Residuals vs Order plot saved as 'residuals_vs_order.png'\n")

# =============================================================================
# 9. FINAL OUTPUT & SUMMARY
# =============================================================================

cat("\n========== FINAL MODEL SUMMARY ==========\n")
cat("Final model:", model_type, "\n")
cat("R-squared:", round(final_summary$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(final_summary$adj.r.squared, 4), "\n")

# Display robust standard errors
cat("\n--- Coefficients with Robust Standard Errors (for Inference) ---\n")
print(final_summary_robust$coefficients)

# =============================================================================
# 10. SAVE FINAL MODEL
# =============================================================================

cat("\n========== SAVING FINAL MODEL ==========\n")

# Save final model
saveRDS(final_model, "final_model.rds")
cat("Final model saved as 'final_model.rds'\n")

# Save model summary
model_summary_list <- list(
  Model = final_model,
  Summary = final_summary,
  Summary_Robust = final_summary_robust,
  Coefficients = coef(final_model),
  R_Squared = final_summary$r.squared,
  Adj_R_Squared = final_summary$adj.r.squared,
  AIC = AIC(final_model),
  Residual_SE = final_summary$sigma,
  Transformation = transformation_type,
  Transformation_Formula = transformation_formula,
  Shift_Value = shift_value,
  Model_Type = model_type,
  Has_Robust_SE = !is.null(final_summary_robust)
)
saveRDS(model_summary_list, "final_model_summary.rds")
cat("Model summary saved as 'final_model_summary.rds'\n")

# Save model equation
coefs <- coef(final_model)
model_equation <- paste(transformation_formula, "=", round(coefs[1], 4))
for(i in 2:length(coefs)) {
  sign <- ifelse(coefs[i] >= 0, " + ", " - ")
  model_equation <- paste0(model_equation, sign, 
                          round(abs(coefs[i]), 4), " * ", names(coefs)[i])
}
writeLines(model_equation, "model_equation.txt")
cat("Model equation saved as 'model_equation.txt'\n")

# Print model equation
cat("\nFinal Model Equation:\n")
cat(model_equation, "\n")
cat("\nNote: To get predictions in original Exam_Score units:\n")
cat("  predicted_score = exp(predicted_transformed) -", shift_value, "\n")

cat("\n========== ANALYSIS COMPLETE ==========\n")
cat("Final model saved with robust standard errors for valid inference.\n")
cat("\nDiagnostic plots created:\n")
cat("  - qq_plot_residuals.png (Normality)\n")
cat("  - cooks_distance_plot.png (Influential observations)\n")
cat("  - residuals_vs_fitted.png (Linearity)\n")
cat("  - residuals_vs_order.png (Independence)\n")
cat("  - scale_location_plot.png (Homoscedasticity)\n")
cat("  - residuals_histogram.png (Normality)\n")
cat("\nAll four linear regression assumptions have been checked.\n")