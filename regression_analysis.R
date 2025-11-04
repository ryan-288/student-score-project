# STA 4164 - Statistical Method III
# Project 2 - Comprehensive Regression Analysis
# Student Performance Factors Analysis

# =============================================================================
# ENVIRONMENT & SETUP
# =============================================================================

# Load required libraries
library(tidyverse)    # Data manipulation and visualization
library(corrplot)     # Correlation matrices
library(car)          # Diagnostic plots and VIF
library(MASS)         # Stepwise regression
library(caret)        # Data splitting
library(gridExtra)    # Arrange plots
library(knitr)        # Tables

# Load rstudioapi if available (for automatic working directory detection)
if (requireNamespace("rstudioapi", quietly = TRUE)) {
  library(rstudioapi)
}

# Set seed for reproducibility
set.seed(4164)

# =============================================================================
# 1. DATA LOADING & INITIAL EXPLORATION
# =============================================================================

# Automatically set working directory to script's location
# This ensures the script finds the CSV file even if RStudio's working directory is different
if (exists("rstudioapi") && rstudioapi::isAvailable()) {
  # If running in RStudio, use the script's directory
  script_path <- rstudioapi::getActiveDocumentContext()$path
  if (script_path != "") {
    setwd(dirname(script_path))
    cat("Working directory set to:", getwd(), "\n")
  }
} else {
  # Try alternative method for RStudio
  tryCatch({
    if (requireNamespace("rstudioapi", quietly = TRUE)) {
      script_path <- rstudioapi::getActiveDocumentContext()$path
      if (script_path != "") {
        setwd(dirname(script_path))
        cat("Working directory set to:", getwd(), "\n")
      }
    }
  }, error = function(e) {
    # If rstudioapi fails, try command line arguments (works with Rscript)
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

# Try to find the file
csv_file <- "StudentPerformanceFactors.csv"
if (!file.exists(csv_file)) {
  # Try with full path as fallback
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

# Initial data structure
str(df)
summary(df)

# Check for missing values
sapply(df, function(x) sum(is.na(x)))

# =============================================================================
# 2. DATA EXPLORATION
# =============================================================================

# 2a. Descriptive Statistics
cat("\n========== DESCRIPTIVE STATISTICS ==========\n")
summary_stats <- df %>%
  select_if(is.numeric) %>%
  summary()
print(summary_stats)

# Create summary table
numeric_vars <- df %>% select_if(is.numeric)
desc_table <- data.frame(
  Variable = names(numeric_vars),
  Mean = round(sapply(numeric_vars, mean, na.rm = TRUE), 2),
  Median = round(sapply(numeric_vars, median, na.rm = TRUE), 2),
  SD = round(sapply(numeric_vars, sd, na.rm = TRUE), 2),
  Min = sapply(numeric_vars, min, na.rm = TRUE),
  Max = sapply(numeric_vars, max, na.rm = TRUE),
  Missing = sapply(numeric_vars, function(x) sum(is.na(x)))
)
print(desc_table)

# =============================================================================
# 2b. VISUALIZATIONS & CORRELATIONS
# =============================================================================

cat("\n========== CORRELATION ANALYSIS ==========\n")

# Calculate correlation matrix for numeric variables
cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(round(cor_matrix, 3))

# Correlation matrix with Exam_Score
if("Exam_Score" %in% names(numeric_vars)) {
  cat("\nCorrelations with Exam_Score:\n")
  exam_cor <- cor_matrix["Exam_Score", ]
  exam_cor_sorted <- sort(exam_cor, decreasing = TRUE)
  print(round(exam_cor_sorted, 3))
}

# Visualize correlation matrix
png("correlation_matrix.png", width = 1000, height = 1000, res = 150)
# Set margins to ensure nothing gets cut off
par(mar = c(2, 2, 4, 2))
corrplot(cor_matrix, method = "color", type = "upper", 
         order = "hclust", tl.cex = 0.8, tl.col = "black",
         addCoef.col = "black", number.cex = 0.7,
         tl.offset = 0.5, tl.srt = 45)
title(main = "Correlation Matrix of Numeric Variables", 
      line = 2.5, cex.main = 1.2)
dev.off()
cat("Correlation matrix saved as 'correlation_matrix.png'\n")

# Scatter plots of numeric variables vs Exam_Score
if("Exam_Score" %in% names(numeric_vars)) {
  cat("\n========== SCATTER PLOTS vs EXAM_SCORE ==========\n")
  
  # Get numeric variables excluding Exam_Score
  predictor_vars <- names(numeric_vars)[names(numeric_vars) != "Exam_Score"]
  
  # Create scatter plots
  scatter_plots <- list()
  for(var in predictor_vars) {
    scatter_plots[[var]] <- ggplot(df, aes(x = !!sym(var), y = Exam_Score)) +
      geom_point(alpha = 0.6, color = "steelblue") +
      geom_smooth(method = "lm", se = TRUE, color = "red") +
      labs(title = paste("Exam Score vs", var),
           x = var, y = "Exam Score") +
      theme_minimal()
  }
  
  # Save individual scatter plots
  for(var in predictor_vars) {
    png(paste0("scatter_", var, "_vs_ExamScore.png"), 
        width = 600, height = 400, res = 150)
    print(scatter_plots[[var]])
    dev.off()
  }
  
  # Create combined scatter plot grid (first 6 predictors)
  if(length(scatter_plots) > 0) {
    n_plots <- min(6, length(scatter_plots))
    scatter_grid <- grid.arrange(grobs = scatter_plots[1:n_plots], 
                                 ncol = 3, 
                                 top = "Scatter Plots: Numeric Variables vs Exam Score")
    
    ggsave("scatter_plots_grid.png", scatter_grid, 
           width = 12, height = 8, dpi = 150)
    cat("Scatter plots saved individually and as 'scatter_plots_grid.png'\n")
  }
}

# Distribution plots
cat("\n========== DISTRIBUTION PLOTS ==========\n")

# Distribution of Exam_Score (if exists)
if("Exam_Score" %in% names(numeric_vars)) {
  png("distribution_ExamScore.png", width = 800, height = 400, res = 150)
  p1 <- ggplot(df, aes(x = Exam_Score)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "steelblue", alpha = 0.7) +
    geom_density(color = "red", linewidth = 1.2) +
    labs(title = "Distribution of Exam Score", x = "Exam Score", y = "Density") +
    theme_minimal()
  p2 <- ggplot(df, aes(y = Exam_Score)) +
    geom_boxplot(fill = "lightblue", alpha = 0.7) +
    labs(title = "Boxplot of Exam Score", y = "Exam Score") +
    theme_minimal()
  dist_plot <- grid.arrange(p1, p2, ncol = 2)
  print(dist_plot)
  dev.off()
  cat("Exam Score distribution plot saved as 'distribution_ExamScore.png'\n")
}

# Distribution plots for all numeric variables
numeric_long <- numeric_vars %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value")

dist_all <- ggplot(numeric_long, aes(x = Value)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "steelblue", alpha = 0.7) +
  geom_density(color = "red", linewidth = 0.8) +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "Distribution of All Numeric Variables", 
       x = "Value", y = "Density") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("distribution_all_variables.png", dist_all, 
       width = 14, height = 10, dpi = 150)
cat("All variable distributions saved as 'distribution_all_variables.png'\n")

# =============================================================================
# 2c. COLLINEARITY EVALUATION (Preliminary)
# =============================================================================

cat("\n========== PRELIMINARY COLLINEARITY CHECK ==========\n")

# High correlation threshold
high_cor_threshold <- 0.7
high_cor_pairs <- which(abs(cor_matrix) > high_cor_threshold & 
                        cor_matrix != 1, arr.ind = TRUE)

if(nrow(high_cor_pairs) > 0) {
  cat("Variables with correlation >", high_cor_threshold, ":\n")
  for(i in 1:nrow(high_cor_pairs)) {
    var1 <- rownames(cor_matrix)[high_cor_pairs[i, 1]]
    var2 <- colnames(cor_matrix)[high_cor_pairs[i, 2]]
    cor_val <- cor_matrix[high_cor_pairs[i, 1], high_cor_pairs[i, 2]]
    cat(sprintf("%s - %s: %.3f\n", var1, var2, cor_val))
  }
} else {
  cat("No pairs with correlation >", high_cor_threshold, "found.\n")
}

# =============================================================================
# 3. DATA PREPARATION
# =============================================================================

cat("\n========== DATA PREPARATION ==========\n")

# 3a. Dealing with Missing Values
cat("\n--- 3a. MISSING VALUES ---\n")
missing_summary <- data.frame(
  Variable = names(df),
  Missing_Count = sapply(df, function(x) sum(is.na(x))),
  Missing_Percent = round(100 * sapply(df, function(x) sum(is.na(x))) / nrow(df), 2)
)
print(missing_summary)

# Remove rows with missing Exam_Score (outcome variable)
data_clean <- df %>% filter(!is.na(Exam_Score))
cat("\nOriginal rows:", nrow(df), "\n")
cat("After removing missing Exam_Score:", nrow(data_clean), "\n")
cat("Rows removed:", nrow(df) - nrow(data_clean), "\n")

# Check remaining missing values
cat("\nRemaining missing values after removing Exam_Score NAs:\n")
missing_after <- sapply(data_clean, function(x) sum(is.na(x)))
print(missing_after[missing_after > 0])

# For other variables, we'll use complete cases for modeling
# (Can be modified to use imputation if needed)

# 3b. Treating Outliers
cat("\n--- 3b. OUTLIER DETECTION ---\n")

# Identify outliers using IQR method for numeric variables
outlier_detection <- function(x, var_name) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  outliers <- which(x < lower | x > upper)
  return(list(count = length(outliers), indices = outliers, 
              lower = lower, upper = upper))
}

outlier_summary <- data.frame(
  Variable = character(),
  Outlier_Count = integer(),
  Lower_Bound = numeric(),
  Upper_Bound = numeric(),
  stringsAsFactors = FALSE
)

for(var in names(numeric_vars)) {
  if(var %in% names(data_clean)) {
    result <- outlier_detection(data_clean[[var]], var)
    if(result$count > 0) {
      outlier_summary <- rbind(outlier_summary, 
                               data.frame(Variable = var,
                                         Outlier_Count = result$count,
                                         Lower_Bound = round(result$lower, 2),
                                         Upper_Bound = round(result$upper, 2),
                                         stringsAsFactors = FALSE))
    }
  }
}

if(nrow(outlier_summary) > 0) {
  print(outlier_summary)
  cat("\nNote: Outliers will be examined during model diagnostics.\n")
  cat("We will use Cook's distance and leverage to identify influential observations.\n")
} else {
  cat("No outliers detected using IQR method.\n")
}

# Boxplots for outlier visualization (only if predictor_vars exists and has elements)
if(exists("predictor_vars") && length(predictor_vars) > 0) {
  outlier_plots <- list()
  plot_count <- 0
  for(var in predictor_vars) {
    if(var %in% names(data_clean) && plot_count < 6) {
      outlier_plots[[var]] <- ggplot(data_clean, aes(y = !!sym(var))) +
        geom_boxplot(fill = "lightcoral", alpha = 0.7) +
        labs(title = paste("Boxplot:", var), y = var) +
        theme_minimal()
      plot_count <- plot_count + 1
    }
  }
  if(length(outlier_plots) > 0) {
    png("outlier_detection_boxplots.png", width = 1200, height = 800, res = 150)
    grid.arrange(grobs = outlier_plots, ncol = 3)
    dev.off()
    cat("Outlier boxplots saved as 'outlier_detection_boxplots.png'\n")
  } else {
    cat("Skipping outlier boxplots - no valid variables found\n")
  }
} else {
  cat("Skipping outlier boxplots - predictor_vars not available\n")
}

# 3c. Possible Transformations
cat("\n--- 3c. TRANSFORMATION EXPLORATION ---\n")
cat("Checking normality of Exam_Score for potential transformation needs...\n")

# Shapiro-Wilk test for normality (on sample if n > 5000)
if(nrow(data_clean) <= 5000) {
  shapiro_result <- shapiro.test(data_clean$Exam_Score)
  cat("Shapiro-Wilk normality test for Exam_Score:\n")
  cat("W =", round(shapiro_result$statistic, 4), 
      ", p-value =", format(shapiro_result$p.value, scientific = TRUE), "\n")
} else {
  cat("Sample size > 5000, using Kolmogorov-Smirnov test on sample:\n")
  sample_exam <- sample(data_clean$Exam_Score, 5000)
  ks_result <- ks.test(sample_exam, "pnorm", mean = mean(sample_exam), sd = sd(sample_exam))
  cat("KS test p-value:", format(ks_result$p.value, scientific = TRUE), "\n")
}

cat("\nNote: Transformations will be considered during model diagnostics\n")
cat("if residuals show non-normality or heteroscedasticity.\n")

# 3d. Creating Dummy Variables
cat("\n--- 3d. DUMMY VARIABLE CREATION ---\n")

# Identify categorical variables
categorical_vars <- data_clean %>%
  select_if(is.character) %>%
  names()

cat("Categorical variables found:", paste(categorical_vars, collapse = ", "), "\n")

# Create dummy variables for categorical predictors
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
cat("Total dummy variables:", sum(grepl("_Medium|_High|_Yes|_No|_Male|_Private|_Positive|_Negative|_College|_Postgrad|_Moderate|_Far", names(data_model))), "\n")

# Prepare final modeling dataset (complete cases only)
# Use dplyr::select explicitly to avoid conflict with MASS::select
model_vars <- data_model %>%
  dplyr::select(Exam_Score, 
         # Continuous variables
         Hours_Studied, Attendance, Sleep_Hours, Previous_Scores,
         Tutoring_Sessions, Physical_Activity,
         # Dummy variables
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
# 4. MODELING
# =============================================================================

cat("\n========== MODELING ==========\n")

# 4a. Univariate Model Exploration
cat("\n--- 4a. UNIVARIATE MODEL EXPLORATION ---\n")

univariate_results <- data.frame(
  Predictor = character(),
  Coefficient = numeric(),
  P_Value = numeric(),
  R_Squared = numeric(),
  Adj_R_Squared = numeric(),
  stringsAsFactors = FALSE
)

predictor_names <- names(model_vars)[names(model_vars) != "Exam_Score"]

for(pred in predictor_names) {
  formula_str <- paste("Exam_Score ~", pred)
  model <- lm(as.formula(formula_str), data = model_vars)
  coef_summary <- summary(model)
  
  univariate_results <- rbind(univariate_results,
    data.frame(
      Predictor = pred,
      Coefficient = round(coef_summary$coefficients[2, 1], 4),
      P_Value = format(coef_summary$coefficients[2, 4], scientific = TRUE),
      R_Squared = round(coef_summary$r.squared, 4),
      Adj_R_Squared = round(coef_summary$adj.r.squared, 4),
      stringsAsFactors = FALSE
    )
  )
}

# Sort by absolute coefficient
univariate_results <- univariate_results %>%
  mutate(Abs_Coefficient = abs(Coefficient)) %>%
  arrange(desc(Abs_Coefficient))

print(univariate_results)
cat("\nTop 5 predictors by coefficient magnitude:\n")
print(head(univariate_results, 5))

# Save univariate results
write.csv(univariate_results, "univariate_model_results.csv", row.names = FALSE)

# 4b. Interaction and Confounding Exploration
cat("\n--- 4b. INTERACTION & CONFOUNDING EXPLORATION ---\n")

# Test some theoretically important interactions
cat("Testing potential interactions:\n")

# Interaction between Hours_Studied and Previous_Scores
interaction1 <- lm(Exam_Score ~ Hours_Studied * Previous_Scores, data = model_vars)
cat("\n1. Hours_Studied * Previous_Scores:\n")
interaction1_coef <- summary(interaction1)$coefficients
if("Hours_Studied:Previous_Scores" %in% rownames(interaction1_coef)) {
  print(interaction1_coef[c("Hours_Studied:Previous_Scores"), , drop = FALSE])
} else {
  cat("Interaction term not in model\n")
}

# Interaction between Attendance and Teacher_High
interaction2 <- lm(Exam_Score ~ Attendance * Teacher_High, data = model_vars)
cat("\n2. Attendance * Teacher_High:\n")
interaction2_coef <- summary(interaction2)$coefficients
if("Attendance:Teacher_High" %in% rownames(interaction2_coef)) {
  print(interaction2_coef[c("Attendance:Teacher_High"), , drop = FALSE])
} else {
  cat("Interaction term not in model\n")
}

# Interaction between Parental_High and Access_High
interaction3 <- lm(Exam_Score ~ Parental_High * Access_High, data = model_vars)
cat("\n3. Parental_High * Access_High:\n")
interaction3_coef <- summary(interaction3)$coefficients
if("Parental_High:Access_High" %in% rownames(interaction3_coef)) {
  print(interaction3_coef[c("Parental_High:Access_High"), , drop = FALSE])
} else {
  cat("Interaction term not in model\n")
}

# Interaction between Motivation_High and Hours_Studied
interaction4 <- lm(Exam_Score ~ Motivation_High * Hours_Studied, data = model_vars)
cat("\n4. Motivation_High * Hours_Studied:\n")
interaction4_coef <- summary(interaction4)$coefficients
if("Motivation_High:Hours_Studied" %in% rownames(interaction4_coef)) {
  print(interaction4_coef[c("Motivation_High:Hours_Studied"), , drop = FALSE])
} else {
  cat("Interaction term not in model\n")
}

cat("\nNote: Significant interactions (p < 0.05) will be considered in the comprehensive model.\n")

# 4c. Comprehensive Model
cat("\n--- 4c. COMPREHENSIVE MODEL ---\n")

# Full model with all predictors
full_model <- lm(Exam_Score ~ ., data = model_vars)
cat("Full model with all predictors:\n")
print(summary(full_model))

cat("\nModel Summary:\n")
cat("R-squared:", round(summary(full_model)$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(summary(full_model)$adj.r.squared, 4), "\n")
cat("F-statistic:", round(summary(full_model)$fstatistic[1], 2), "\n")
cat("p-value:", format(pf(summary(full_model)$fstatistic[1],
                          summary(full_model)$fstatistic[2],
                          summary(full_model)$fstatistic[3],
                          lower.tail = FALSE), scientific = TRUE), "\n")

# 4d. Model Selection
cat("\n--- 4d. MODEL SELECTION ---\n")

# Stepwise selection (both directions)
cat("Performing stepwise selection (both directions)...\n")
null_model <- lm(Exam_Score ~ 1, data = model_vars)
best_model_step <- stepAIC(null_model, 
                           direction = "both",
                           scope = list(upper = full_model, lower = null_model),
                           trace = 0)

cat("\nBest model from stepwise selection:\n")
print(summary(best_model_step))
cat("\nR-squared:", round(summary(best_model_step)$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(summary(best_model_step)$adj.r.squared, 4), "\n")
cat("Number of predictors:", length(coef(best_model_step)) - 1, "\n")

# Compare models using AIC
cat("\nModel Comparison (AIC):\n")
cat("Full model AIC:", round(AIC(full_model), 2), "\n")
cat("Stepwise model AIC:", round(AIC(best_model_step), 2), "\n")
cat("Improvement:", round(AIC(full_model) - AIC(best_model_step), 2), "points\n")

# Identify which variables were removed
full_vars <- names(coef(full_model))[-1]  # Exclude intercept
stepwise_vars <- names(coef(best_model_step))[-1]  # Exclude intercept
removed_vars <- setdiff(full_vars, stepwise_vars)

if(length(removed_vars) > 0) {
  cat("\nVariables removed by stepwise selection:\n")
  for(var in removed_vars) {
    # Get p-value from full model to show why it was removed
    full_coef <- summary(full_model)$coefficients
    if(var %in% rownames(full_coef)) {
      p_val <- full_coef[var, 4]
      cat("  -", var, "(p-value in full model:", format(p_val, scientific = TRUE), ")\n")
    } else {
      cat("  -", var, "\n")
    }
  }
  cat("Total variables removed:", length(removed_vars), "\n")
} else {
  cat("\nNo variables were removed - stepwise kept all predictors.\n")
}

cat("\nVariables kept in final model:", length(stepwise_vars), "out of", length(full_vars), "\n")

# Save best model
best_model <- best_model_step

# 4e. Model Diagnostics
cat("\n--- 4e. MODEL DIAGNOSTICS ---\n")

# Normality of residuals
cat("\nResidual Normality Test:\n")
residuals <- residuals(best_model)
if(length(residuals) <= 5000) {
  shapiro_resid <- shapiro.test(residuals)
  cat("Shapiro-Wilk test: W =", round(shapiro_resid$statistic, 4),
      ", p-value =", format(shapiro_resid$p.value, scientific = TRUE), "\n")
} else {
  cat("Sample size > 5000, using KS test on sample:\n")
  sample_resid <- sample(residuals, 5000)
  ks_resid <- ks.test(sample_resid, "pnorm", mean = 0, sd = sd(residuals))
  cat("KS test p-value:", format(ks_resid$p.value, scientific = TRUE), "\n")
}

# VIF for multicollinearity
cat("\nVariance Inflation Factors (VIF):\n")
vif_values <- vif(best_model)
vif_table <- data.frame(
  Variable = names(vif_values),
  VIF = round(vif_values, 2)
)
vif_table <- vif_table[order(-vif_table$VIF), ]
print(vif_table)

# Check for high VIF (> 10 indicates multicollinearity)
high_vif <- vif_table[vif_table$VIF > 10, ]
if(nrow(high_vif) > 0) {
  cat("\nWARNING: Variables with VIF > 10 (multicollinearity concern):\n")
  print(high_vif)
} else {
  cat("\nNo multicollinearity concerns (all VIF < 10).\n")
}

# Influential observations
cat("\nInfluential Observations:\n")
cooks_d <- cooks.distance(best_model)
influential <- which(cooks_d > 4 / nrow(model_vars))
cat("Observations with Cook's distance > 4/n:", length(influential), "\n")
if(length(influential) > 0) {
  cat("Indices:", paste(head(influential, 10), collapse = ", "))
  if(length(influential) > 10) cat(", ...")
  cat("\n")
}

# Leverage points
leverage <- hatvalues(best_model)
high_leverage <- which(leverage > 2 * mean(leverage))
cat("High leverage points (leverage > 2*mean):", length(high_leverage), "\n")

# Standardized residuals
std_residuals <- rstandard(best_model)
outliers_resid <- which(abs(std_residuals) > 3)
cat("Outliers (|standardized residual| > 3):", length(outliers_resid), "\n")

# Investigate influential observations
if(length(influential) > 0) {
  cat("\nInvestigating influential observations:\n")
  influential_data <- model_vars[influential, ]
  cat("Summary statistics for influential observations:\n")
  cat("Exam_Score range:", min(influential_data$Exam_Score), "-", max(influential_data$Exam_Score), "\n")
  cat("Mean Exam_Score (influential):", round(mean(influential_data$Exam_Score), 2), 
      "vs Overall mean:", round(mean(model_vars$Exam_Score), 2), "\n")
  
  # Check for extreme values
  extreme_scores <- sum(influential_data$Exam_Score > 90 | influential_data$Exam_Score < 60)
  cat("Influential observations with extreme Exam_Scores (>90 or <60):", extreme_scores, "\n")
  
  # Save influential observations for review
  write.csv(influential_data, "influential_observations.csv", row.names = FALSE)
  cat("Influential observations saved to 'influential_observations.csv'\n")
}

# Save diagnostic summary
diagnostic_summary <- list(
  Residual_Normality = if(length(residuals) <= 5000) {
    shapiro.test(residuals)$p.value
  } else {
    ks.test(sample(residuals, 5000), "pnorm", mean = 0, sd = sd(residuals))$p.value
  },
  VIF_Table = vif_table,
  Influential_Count = length(influential),
  High_Leverage_Count = length(high_leverage),
  Outlier_Residual_Count = length(outliers_resid)
)
saveRDS(diagnostic_summary, "diagnostic_summary.rds")

# 4f. Final Best Model
cat("\n--- 4f. FINAL BEST MODEL ---\n")

cat("\nFinal Model Equation:\n")
cat("Exam_Score = ")
coefs <- coef(best_model)
cat(round(coefs[1], 4), "\n")
for(i in 2:length(coefs)) {
  sign <- ifelse(coefs[i] >= 0, "+", "")
  cat("  ", sign, round(coefs[i], 4), " * ", names(coefs)[i], "\n", sep = "")
}

cat("\nFinal Model Summary:\n")
final_summary <- summary(best_model)
print(final_summary)

cat("\nModel Performance Metrics:\n")
cat("R-squared:", round(final_summary$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(final_summary$adj.r.squared, 4), "\n")
cat("Residual Standard Error:", round(final_summary$sigma, 2), "\n")
cat("F-statistic:", round(final_summary$fstatistic[1], 2), "\n")
cat("p-value:", format(pf(final_summary$fstatistic[1],
                          final_summary$fstatistic[2],
                          final_summary$fstatistic[3],
                          lower.tail = FALSE), scientific = TRUE), "\n")

cat("\nSignificant Predictors (p < 0.05):\n")
sig_coefs <- final_summary$coefficients
sig_vars <- rownames(sig_coefs)[sig_coefs[, 4] < 0.05 & rownames(sig_coefs) != "(Intercept)"]
sig_table <- data.frame(
  Predictor = sig_vars,
  Coefficient = round(sig_coefs[sig_vars, 1], 4),
  P_Value = format(sig_coefs[sig_vars, 4], scientific = TRUE)
)
print(sig_table)

# Save final model summary
final_model_summary <- list(
  Model = best_model,
  Summary = final_summary,
  Coefficients = coef(best_model),
  Significant_Predictors = sig_table,
  R_Squared = final_summary$r.squared,
  Adj_R_Squared = final_summary$adj.r.squared,
  Residual_SE = final_summary$sigma
)
saveRDS(final_model_summary, "final_model_summary.rds")

# Create actual vs predicted plot for full dataset
cat("\nCreating Actual vs Predicted plot for full dataset...\n")
all_predictions <- predict(best_model, newdata = model_vars)
actual_vs_predicted <- data.frame(
  Actual = model_vars$Exam_Score,
  Predicted = all_predictions
)

png("actual_vs_predicted_plot.png", width = 900, height = 700, res = 150)
p1 <- ggplot(actual_vs_predicted, aes(x = Predicted, y = Actual)) +
  geom_point(alpha = 0.5, color = "steelblue", size = 1) +
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1.5, linetype = "dashed") +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen", linewidth = 1) +
  labs(title = "Predicted vs Actual Exam Scores (Full Dataset)",
       subtitle = paste("R² =", round(cor(actual_vs_predicted$Actual, actual_vs_predicted$Predicted)^2, 3)),
       x = "Predicted Exam Score", 
       y = "Actual Exam Score") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12))
print(p1)
dev.off()
cat("Actual vs Predicted plot saved as 'actual_vs_predicted_plot.png'\n")

# =============================================================================
# 5. MODEL PERFORMANCE & EVALUATION
# =============================================================================

cat("\n========== MODEL PERFORMANCE & EVALUATION ==========\n")

# Train-test split (80-20)
set.seed(4164)
trainIndex <- createDataPartition(model_vars$Exam_Score, p = 0.8, list = FALSE)
train_data <- model_vars[trainIndex, ]
test_data <- model_vars[-trainIndex, ]

cat("Training set size:", nrow(train_data), "\n")
cat("Test set size:", nrow(test_data), "\n")

# Train model on training set
vars_in_model <- names(coef(best_model))[-1]  # Exclude intercept
train_vars <- c("Exam_Score", vars_in_model)
train_model_data <- train_data[, train_vars]

# Refit model on training data
train_model <- lm(Exam_Score ~ ., data = train_model_data)

# Predictions on test set
test_vars <- test_data[, vars_in_model, drop = FALSE]
predictions <- predict(train_model, newdata = test_vars)

# Performance metrics
cat("\nModel Performance on Test Set:\n")
mse <- mean((test_data$Exam_Score - predictions)^2)
rmse <- sqrt(mse)
mae <- mean(abs(test_data$Exam_Score - predictions))
r_squared_test <- cor(test_data$Exam_Score, predictions)^2

cat("Mean Squared Error (MSE):", round(mse, 2), "\n")
cat("Root Mean Squared Error (RMSE):", round(rmse, 2), "\n")
cat("Mean Absolute Error (MAE):", round(mae, 2), "\n")
cat("R-squared (test set):", round(r_squared_test, 4), "\n")

# Prediction plot - Predicted vs Actual (standard convention)
png("model_prediction_plot.png", width = 800, height = 600, res = 150)
pred_df <- data.frame(Actual = test_data$Exam_Score, Predicted = predictions)
p2 <- ggplot(pred_df, aes(x = Predicted, y = Actual)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1.2) +
  labs(title = "Predicted vs Actual Exam Scores (Test Set)",
       x = "Predicted Exam Score", y = "Actual Exam Score") +
  theme_minimal()
print(p2)
dev.off()
cat("Prediction plot saved as 'model_prediction_plot.png'\n")

# Save performance metrics
performance_metrics <- list(
  MSE = mse,
  RMSE = rmse,
  MAE = mae,
  R_Squared_Test = r_squared_test
)
saveRDS(performance_metrics, "performance_metrics.rds")

# =============================================================================
# 6. SAVE WORKSPACE & OUTPUT
# =============================================================================

cat("\n========== SAVING RESULTS ==========\n")

# Save workspace (optional - contains all variables for future use)
# Uncomment the next 2 lines if you want to save the full workspace
# save.image("regression_analysis_results.RData")
# cat("Workspace saved as 'regression_analysis_results.RData'\n")

# Save final model
saveRDS(best_model, "final_model.rds")
cat("Final model saved as 'final_model.rds'\n")

# Save model equation as text
model_equation <- paste("Exam_Score =", round(coefs[1], 4))
for(i in 2:length(coefs)) {
  sign <- ifelse(coefs[i] >= 0, " + ", " - ")
  model_equation <- paste0(model_equation, sign, 
                          round(abs(coefs[i]), 4), " * ", names(coefs)[i])
}
writeLines(model_equation, "model_equation.txt")
cat("Model equation saved as 'model_equation.txt'\n")

cat("\n========== ANALYSIS COMPLETE ==========\n")
cat("All plots, results, and models have been saved.\n")
cat("\nGenerated files:\n")
cat("- correlation_matrix.png\n")
cat("- scatter_plots_*.png\n")
cat("- distribution_*.png\n")
cat("- outlier_detection_boxplots.png\n")
cat("- actual_vs_predicted_plot.png\n")
cat("- model_prediction_plot.png\n")
cat("- univariate_model_results.csv\n")
cat("- final_model.rds\n")
cat("- final_model_summary.rds\n")
cat("- diagnostic_summary.rds\n")
cat("- performance_metrics.rds\n")
cat("- model_equation.txt\n")

