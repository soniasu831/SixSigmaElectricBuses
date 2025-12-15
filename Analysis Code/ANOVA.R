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
# Usage (with uploaded data) 
# =====================================================
df <- read.csv("buses_with_price_per_seat_and_converted_dates.csv")

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

price_per_seat_results <-run_single_anova(df, response_col = "base_price", factor_col = "price_per_seat")

## this didn't wokr correctly to Split the data by state and then run ANOVA by another variable
# state_anova_results <- df %>%
#   split(.$state) %>%
#   lapply(function(sub_df) {
#     cat("\n\n=============================\n")
#     cat("State:", unique(sub_df$state), "\n")
#     cat("=============================\n")
#     run_single_anova(
#       sub_df,
#       response_col = "base_price",
#       factor_col = "bus_manufacturer"
#     )
#   })

## this didn't work correctly to make a summary table
# Create a summary data frame
# 
# summary_df <- data.frame(
#   Factor = factor_col,
#   DF_Factor = summary_out[[1]]["Df"][1],
#   DF_Residuals = summary_out[[1]]["Df"][2],
#   F_value = summary_out[[1]]["F value"][1],
#   P_value = p_val,
#   Shapiro_p = shapiro_p,
#   Levene_p = levene_p,
#   Significant = ifelse(p_val < alpha, "✅ Significant", "❌ Not significant"),
#   stringsAsFactors = FALSE
# )
# 
# return(summary_df)

## this doesn't work to print a nice table
# library(gridExtra)
# library(grid)
# 
# # Function to save ANOVA summary as image
# save_anova_summary_image <- function(model, shapiro_p, levene_p, significant_msg, title, filename) {
#   
#   # Extract ANOVA summary as data frame
#   summary_df <- as.data.frame(summary(model)[[1]])
#   summary_df <- tibble::rownames_to_column(summary_df, "Term")
#   
#   # Add assumption tests and significance message as extra rows
#   extra_info <- data.frame(
#     Term = c("Shapiro-Wilk p", "Levene p", "Result"),
#     Df = c("", "", ""),
#     `Sum Sq` = c("", "", ""),
#     `Mean Sq` = c("", "", ""),
#     `F value` = c("", "", ""),
#     `Pr(>F)` = c(shapiro_p, levene_p, significant_msg),
#     stringsAsFactors = FALSE
#   )
#   
#   final_df <- rbind(summary_df, extra_info)
#   
#   # Open PNG device
#   png(filename, width = 5000, height = 1200, res = 150)
#   
#   # Create table grob
#   table_grob <- tableGrob(final_df, rows = NULL)
#   
#   # Add title
#   title_grob <- textGrob(title, gp = gpar(fontsize = 18, fontface = "bold"))
#   
#   # Arrange and save
#   grid.arrange(title_grob, table_grob, ncol = 1, heights = c(0.15, 1))
#   
#   dev.off()
# }
# 
# # Example usage after running ANOVA
# res <- run_single_anova(df, response_col = "base_price", factor_col = "state")
# model <- res$model
# shapiro_p <- res$shapiro_p
# levene_p <- res$levene_p
# significant_msg <- ifelse(res$p_val < 0.05, "✅ Significant", "❌ Not significant")
# 
# save_anova_summary_image(model, shapiro_p, levene_p, significant_msg,
#                          "ANOVA Summary: State", "anova_state_summary.png")

# VERSION 1 OF PULLING ANOVA AND TUKEY INTO HTML, WORKED TO PULL ANOVA BUT NOT TUKEY
# library(texreg)
# 
# # Create a named list of all your ANOVA results
# anova_results <- list(
#   state = state_results,
#   source_type = source_results,
#   purchase_year = purchasey_results,
#   bus_manufacturer = manufacturer_results,
#   bus_model = model_results,
#   bus_type = type_results,
#   seating_capacity = seating_results,
#   special_needs_bus = specialneeds_results,
#   vehicle_dealer = dealer_results,
#   price_per_seat = price_per_seat_results
# )
# 
# # Loop through each result and create an HTML file
# for (name in names(anova_results)) {
#   result <- anova_results[[name]]
#   
#   # Safely extract F-statistic and p-value
#   f_value <- tryCatch({
#     f <- summary(result$model)[[1]][["F value"]][1]
#     if (is.null(f) || is.na(f)) NA_real_ else as.numeric(f)
#   }, error = function(e) NA_real_)
#   
#   p_value <- tryCatch({
#     p <- result$p_val
#     if (is.null(p) || is.na(p)) NA_real_ else as.numeric(p)
#   }, error = function(e) NA_real_)
#   
#   # Create texreg object
#   custom_model <- createTexreg(
#     coef.names = c(paste("Effect of", name)),
#     coef = c(f_value),
#     se = c(0),  # Use 0 or a dummy value if SE is not available
#     pvalues = c(p_value),
#     gof.names = c("Shapiro-Wilk p", "Levene p"),
#     gof = c(result$shapiro_p, result$levene_p)
#   )
#   
#   # Save HTML file
#   htmlreg(
#     custom_model,
#     file = paste0("anova_", name, ".html"),
#     caption = paste("ANOVA Results for", name),
#     inline.css = TRUE
#   )
# }
# 
# for (name in names(anova_results)) {
#   result <- anova_results[[name]]
#   
#   f_value <- tryCatch(summary(result$model)[[1]][["F value"]][1], error = function(e) NA_real_)
#   p_value <- result$p_val
#   
#   # Create texreg object
#   custom_model <- createTexreg(
#     coef.names = c(paste("Effect of", name)),
#     coef = c(f_value),
#     se = c(0),
#     pvalues = c(p_value),
#     gof.names = c("Shapiro-Wilk p", "Levene p"),
#     gof = c(result$shapiro_p, result$levene_p)
#   )
#   
#   # Save HTML file
#   htmlreg(
#     custom_model,
#     file = paste0("anova_", name, ".html"),
#     caption = paste("ANOVA Results for", name),
#     inline.css = TRUE
#   )
#   
#   # Print Tukey HSD table if significant
#   if (!is.na(p_value) && p_value < 0.05) {
#     cat("\nTukey HSD for", name, ":\n")
#     print(TukeyHSD(result$model))
#   }
# }

library(texreg)

anova_results <- list(
  state = state_results,
  source_type = source_results,
  purchase_year = purchasey_results,
  bus_manufacturer = manufacturer_results,
  bus_model = model_results,
  bus_type = type_results,
  seating_capacity = seating_results,
  special_needs_bus = specialneeds_results,
  vehicle_dealer = dealer_results,
  price_per_seat = price_per_seat_results
)

for (name in names(anova_results)) {
  result <- anova_results[[name]]
  
  f_value <- tryCatch(summary(result$model)[[1]][["F value"]][1], error = function(e) NA_real_)
  p_value <- result$p_val
  
  custom_model <- createTexreg(
    coef.names = c(paste("Effect of", name)),
    coef = c(f_value),
    se = c(0),
    pvalues = c(p_value),
    gof.names = c("Shapiro-Wilk p", "Levene p"),
    gof = c(result$shapiro_p, result$levene_p)
  )
  
  # Write ANOVA summary to a temporary HTML file
  temp_file <- tempfile(fileext = ".html")
  htmlreg(custom_model, file = temp_file, caption = paste("ANOVA Results for", name), inline.css = TRUE)
  anova_html <- paste(readLines(temp_file), collapse = "\n")
  
  # Add Tukey HSD if significant
  tukey_html <- ""
  if (!is.na(p_value) && p_value < 0.05) {
    tukey <- TukeyHSD(result$model)
    tukey_df <- as.data.frame(tukey[[1]])
    tukey_df$Comparison <- rownames(tukey_df)
    
    # Create HTML table
    tukey_html <- paste0(
      "<h3>Tukey HSD Results for ", name, "</h3>",
      "<table border='1' cellpadding='5' cellspacing='0'><tr>",
      paste0("<th>", colnames(tukey_df), "</th>", collapse = ""),
      "</tr>",
      paste(apply(tukey_df, 1, function(row) {
        paste0("<tr>", paste0("<td>", format(row, digits = 4), "</td>", collapse = ""), "</tr>")
      }), collapse = ""),
      "</table>"
    )
  }
  
  # Combine and write to final HTML file
  full_html <- paste0("<html><body>", anova_html, tukey_html, "</body></html>")
  writeLines(full_html, paste0("anova_", name, ".html"))
}
