#' @title Grouped ANOVA Significance Summary for Bus Pricing
#' @description
#' Runs one-way ANOVAs across subgroups (e.g., by state), testing a chosen factor (e.g., manufacturer)
#' against a numeric response (e.g., base price). Automatically returns a table summarizing
#' which groups have significant differences and prints assumption test results.
#'
#' @param df Data frame containing the bus dataset.
#' @param group_by Character. Column to group by (e.g. "state").
#' @param factor_col Character. Column to use as the categorical factor in ANOVA (e.g. "bus_manufacturer").
#' @param response_col Character. Response variable (default = "base_price").
#' @param alpha Numeric. Significance threshold (default = 0.05).
#' 
#' @return A data frame summarizing group, p-value, and whether the ANOVA was significant.
#' @examples
#' results_table <- run_grouped_anova(df, group_by = "state",
#'                                    factor_col = "bus_manufacturer",
#'                                    response_col = "base_price")
#' @export

run_grouped_anova <- function(df, group_by, factor_col, response_col = "base_price", alpha = 0.05) {
  
  library(dplyr)
  library(ggplot2)
  library(car)
  library(ggtext)
  
  # --- Column checks --------------------------------------------------
  if (!all(c(group_by, factor_col, response_col) %in% names(df))) {
    stop("One or more specified columns not found in dataset.")
  }
  
  df[[group_by]]  <- as.factor(df[[group_by]])
  df[[factor_col]] <- as.factor(df[[factor_col]])
  
  grouped_data <- split(df, df[[group_by]])
  
  # --- Output results table ------------------------------------------
  results <- data.frame(
    Group = character(),
    DF_Factor = numeric(),
    DF_Residuals = numeric(),
    F_value = numeric(),
    P_value = numeric(),
    Shapiro_p = numeric(),
    Levene_p = numeric(),
    Significant = logical(),
    stringsAsFactors = FALSE
  )
  
  # --- Loop through groups -------------------------------------------
  for (g in names(grouped_data)) {
    sub_df <- grouped_data[[g]]
    
    # Skip groups without enough data
    if (nrow(sub_df) < 5 || nlevels(sub_df[[factor_col]]) < 2) next
    
    formula <- as.formula(paste0("`", response_col, "` ~ `", factor_col, "`"))
    model <- tryCatch(aov(formula, data = sub_df), error = function(e) NULL)
    if (is.null(model)) next
    
    summary_out <- tryCatch(summary(model)[[1]], error = function(e) NULL)
    if (is.null(summary_out)) next
    
    p_val <- summary_out[1, "Pr(>F)"]
    f_val <- summary_out[1, "F value"]
    df_factor <- summary_out[1, "Df"]
    df_res <- summary_out["Residuals", "Df"]
    
    shapiro_p <- tryCatch(shapiro.test(residuals(model))$p.value, error = function(e) NA)
    levene_p  <- tryCatch(leveneTest(formula, data = sub_df)[["Pr(>F)"]][1], error = function(e) NA)
    
    # store results
    results <- rbind(
      results,
      data.frame(
        Group = g,
        DF_Factor = df_factor,
        DF_Residuals = df_res,
        F_value = f_val,
        P_value = p_val,
        Shapiro_p = shapiro_p,
        Levene_p = levene_p,
        Significant = !is.na(p_val) && p_val < alpha,
        stringsAsFactors = FALSE
      )
    )
  }
  
  # --- Add p-values back to df for labeling on plot -------------------
  df <- df %>%
    left_join(
      results %>% select(Group, P_value) %>%
        rename(!!group_by := Group),
      by = group_by
    )
  
  df$p_label <- paste0("p = ", formatC(df$P_value, digits = 3, format = "e"))
  
  # --- Faceted boxplot ------------------------------------------------
  p <- ggplot(df, aes_string(x = factor_col, y = response_col, fill = factor_col)) +
    geom_boxplot(alpha = 0.7, outlier.color = "red") +
    facet_wrap(as.formula(paste("~", group_by))) +
    geom_text(
      aes(label = p_label),
      x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2,
      size = 3, color = "black"
    ) +
    theme_minimal(base_size = 13) +
    labs(
      title = paste("Grouped ANOVA by", group_by),
      subtitle = paste("Factor tested:", factor_col),
      x = factor_col,
      y = response_col
    ) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = -90, hjust = 0, size = 8),
      strip.text = element_text(face = "bold", size = 12),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
      panel.spacing = unit(1.2, "lines")
    )
  
  print(p)
  
  # --- Final output table ---------------------------------------------
  results <- results %>%
    arrange(P_value) %>%
    mutate(Significance = ifelse(Significant, "✅ Significant", "❌ Not significant"))
  
  print(results)
  invisible(results)
}



# Load your data
df <- read.csv("buses_with_price_per_seat_and_converted_dates.csv")

# Example 1: Run ANOVA by state within each manufacturer
results_table_1 <- run_grouped_anova(
  df,
  group_by = "bus_manufacturer",
  factor_col = "state",
  response_col = "base_price"
)

# Example 2: Run ANOVA by state within each type
results_table_2 <- run_grouped_anova(
  df,
  group_by = "bus_type",
  factor_col = "state",
  response_col = "base_price"
)

# Example 3: Run ANOVA by bus type within if special needs
results_table_3 <- run_grouped_anova(
  df,
  group_by = "special_needs_bus",
  factor_col = "bus_type",
  response_col = "base_price"
)

# Example 4: Run ANOVA by state within each year
results_table_4 <- run_grouped_anova(
  df,
  group_by = "purchase_year",
  factor_col = "bus_type",
  response_col = "base_price"
)

# Example 5: Run ANOVA by bus manufacturer within each year
results_table_5 <- run_grouped_anova(
  df,
  group_by = "purchase_year",
  factor_col = "bus_manufacturer",
  response_col = "base_price"
)

# Example 6: Run ANOVA by bus manufacturer within each state
results_table_6 <- run_grouped_anova(
  df,
  group_by = "state",
  factor_col = "bus_type",
  response_col = "base_price"
)

# Example 7: Run ANOVA by bus manufacturer within each type; return price/seat
results_table_7 <- run_grouped_anova(
  df,
  group_by = "bus_type",
  factor_col = "state",
  response_col = "price_per_seat"
)

# Install required packages if not already installed
# install.packages(c("ggplot2", "gridExtra"))

library(ggplot2)
library(grid)
library(gridExtra)

# Function to save a data frame as an image
save_table_as_image <- function(df, title, filename) {
  
  # Open a PNG device with larger width
  png(filename, width = 5000, height = 1200, res = 600)  # Increased width and resolution
  
  # Create a table grob
  table_grob <- tableGrob(df, rows = NULL)
  
  # Add a title
  title_grob <- textGrob(title, gp = gpar(fontsize = 16, fontface = "bold"))
  
  # Combine title and table
  combined <- arrangeGrob(title_grob, table_grob, ncol = 1, heights = c(0.05, 1))
  
  # Save as PNG
  ggsave(filename, combined, width = 8, height = 38)
}

# Example usage for your results
save_table_as_image(results_table_1, "ANOVA by State within Manufacturer", "anova_table_1.png")
save_table_as_image(results_table_2, "ANOVA by State within Type", "anova_table_2.png")
save_table_as_image(results_table_3, "ANOVA by Bus Type within Special Needs", "anova_table_3.png")
save_table_as_image(results_table_4, "ANOVA by State within Year", "anova_table_4.png")
save_table_as_image(results_table_5, "ANOVA by Manufacturer within Year", "anova_table_5.png")
save_table_as_image(results_table_6, "ANOVA by Bus Type within State", "anova_table_6.png")
save_table_as_image(results_table_7, "ANOVA by Price/Seat for Bus Manufacturer within type", "anova_table_7.png")
save_table_as_image(tukey_table, "Tukey Post-hoc test of Significant Bus Types", "Tukey_table.png")

# TUKEY POST HOC
# Assuming you already have your data frame `df`
# and you grouped by special_needs_bus, factor = bus_type, response = base_price

library(dplyr)

# Identify groups where ANOVA was significant
significant_groups <- results_table_2 %>%
  filter(Significant == TRUE) %>%
  pull(Group)

# Loop through significant groups and run Tukey HSD
posthoc_results <- list()

for (g in significant_groups) {
  sub_df <- df %>% filter(bus_type == g)
  
  # Fit ANOVA model
  model <- aov(base_price ~ state, data = sub_df)
  
  # Tukey HSD post-hoc test
  tukey_out <- TukeyHSD(model)
  
  posthoc_results[[g]] <- tukey_out
}

# View results for each group
posthoc_results

library(broom)

tukey_table <- bind_rows(
  lapply(names(posthoc_results), function(g) {
    tidy(posthoc_results[[g]]) %>%
      mutate(Group = g)
  })
)

print(tukey_table) %>% print(n = 120)
