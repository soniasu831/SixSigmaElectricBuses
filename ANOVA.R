#' @title Single-Column ANOVA for Bus Pricing
#' @description
#' Performs ANOVA for a single categorical variable against the response.
#' Checks assumptions and generates a boxplot.
#' 
#' @param df Data frame containing your bus dataset
#' @param response_col Character. Response variable (default "base_price")
#' @param factor_col Character. Column to test as categorical factor
#' @param alpha Significance threshold (default 0.05)
#' @return List with ANOVA model, p-value, Shapiro-Wilk and Levene tests
#' @export

# install.packages(c("dplyr", "car", "ggplot2"))

run_single_anova <- function(df, response_col = "base_price", factor_col, alpha = 0.05) {
  
  library(dplyr)
  library(ggplot2)
  library(car)
  
  # Check columns exist
  if (!response_col %in% names(df)) stop("Response column not found!")
  if (!factor_col %in% names(df)) stop("Factor column not found!")
  
  # Convert factor column to factor if needed
  df[[factor_col]] <- as.factor(df[[factor_col]])
  
  # Skip if only one level
  if (nlevels(df[[factor_col]]) < 2) {
    stop("Factor column must have at least 2 levels.")
  }
  
  # Build formula
  formula <- as.formula(paste("`", response_col, "` ~ `", factor_col, "`", sep = ""))
  
  # Fit ANOVA
  model <- aov(formula, data = df)
  summary_out <- summary(model)
  print(summary_out)
  
  # Extract p-value
  p_val <- summary_out[[1]][["Pr(>F)"]][1]
  
  # Assumption tests
  shapiro_p <- shapiro.test(residuals(model))$p.value
  levene_p <- car::leveneTest(formula, data = df)[["Pr(>F)"]][1]
  
  cat(sprintf("Shapiro-Wilk p = %.4f\n", shapiro_p))
  cat(sprintf("Levene p = %.4f\n", levene_p))
  
  # Tukey HSD if significant
  if (p_val < alpha) {
    cat("✅ Significant difference detected!\n")
    print(TukeyHSD(model))
  } else {
    cat("❌ No significant difference detected.\n")
  }
  
  # Boxplot
  p <- ggplot(df, aes_string(x = factor_col, y = response_col, fill = factor_col)) +
    geom_boxplot(alpha = 0.7, outlier.color = "red") +
    theme_minimal(base_size = 13) +
    labs(
      title = paste(response_col, "by", factor_col),
      subtitle = paste("ANOVA p =", round(p_val, 4)),
      x = factor_col,
      y = response_col
    ) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1),
      plot.margin = margin(t = 15, r = 120, b = 25, l = 15)
    )
  
  print(p)
  
  # Return results
  invisible(list(
    model = model,
    p_val = p_val,
    shapiro_p = shapiro_p,
    levene_p = levene_p
  ))
}

# ===================================================== 
# Usage (with your uploaded data) 
# =====================================================
df <- read.csv("buses.csv")

# Run ANOVA just for the 'state' column
state_results <- run_single_anova(df, response_col = "base_price", factor_col = "state")

# Run ANOVA just for each column
source_results <- run_single_anova(df, response_col = "base_price", factor_col = "source_type")

purchasey_results <- run_single_anova(df, response_col = "base_price", factor_col = "purchase_year")

manufacturer_results <- run_single_anova(df, response_col = "base_price", factor_col = "bus_manufacturer")

model_results <- run_single_anova(df, response_col = "base_price", factor_col = "bus_model")

type_results <- run_single_anova(df, response_col = "base_price", factor_col = "bus_type")

seating_results <- run_single_anova(df, response_col = "base_price", factor_col = "seating_capacity")

specialneeds_results <- run_single_anova(df, response_col = "base_price", factor_col = "special_needs_bus")

dealer_results <- run_single_anova(df, response_col = "base_price", factor_col = "vehicle_dealer")
