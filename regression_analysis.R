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

# Set seed for reproducibility
set.seed(4164)

# =============================================================================
# 1. DATA LOADING & INITIAL EXPLORATION
# =============================================================================

# Load the data
data <- read.csv("StudentPerformanceFactors.csv")

# Initial data structure
str(data)
summary(data)

# Check for missing values
sapply(data, function(x) sum(is.na(x)))

# =============================================================================
# 2. DATA EXPLORATION
# =============================================================================

# 2a. Descriptive Statistics
cat("\n========== DESCRIPTIVE STATISTICS ==========\n")
summary_stats <- data %>%
  select_if(is.numeric) %>%
  summary()
print(summary_stats)

# Create summary table
numeric_vars <- data %>% select_if(is.numeric)
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
png("correlation_matrix.png", width = 800, height = 800, res = 150)
corrplot(cor_matrix, method = "color", type = "upper", 
         order = "hclust", tl.cex = 0.7, tl.col = "black",
         addCoef.col = "black", number.cex = 0.6,
         title = "Correlation Matrix of Numeric Variables")
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
    scatter_plots[[var]] <- ggplot(data, aes(x = !!sym(var), y = Exam_Score)) +
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
  p1 <- ggplot(data, aes(x = Exam_Score)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "steelblue", alpha = 0.7) +
    geom_density(color = "red", linewidth = 1.2) +
    labs(title = "Distribution of Exam Score", x = "Exam Score", y = "Density") +
    theme_minimal()
  p2 <- ggplot(data, aes(y = Exam_Score)) +
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
