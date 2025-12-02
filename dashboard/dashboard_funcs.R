# %% load in libraries #####
library(dplyr) # data wrangling
library(readr) # reading csv file
library(tidyr) # more data wrangling
library(janitor) # easy crosstabs
library(ggpubr) # plot stacking
library(stringr) # text wrapping
library(ggplot2)

# load in tim process control functions
source("tim_functions_process_control.R")

# %% spc functions #####

#' helper function to convert categorical groups to numerical groups
#' @param name a vector of categorial grouping data
#' @return a vector of integers where each value corresponds to one of the input categories
numgroup = function(name){
  g = c()
  for (i in 1:length(name)){
    g[i] = match(name[i], unique(name))
  }
  return(g)
}


count_numgroup = function(ng){
  
}


#' helper function to create legend for when categorical groups need
#' to be represented as numerical groups
#' @param name a vector, the original categorical grouping data
#' @param number the output of numgroup()
#' @import dplyr
#' @import stringr
group_label = function(name, number) {

  a = table(number)
  b = a[match(number, names(a))] %>% as.numeric()

  label = tibble(
    name = paste0(name, " (", b, ")"),
    number = number
  ) 

  output = label
}

#' @name ggxbar
#' @title Average Control Chart with ggplot
#' @param x [numeric] vector of subgroup values (usually time). Must be same length as `y`.
#' @param y [numeric] vector of metric values (eg. performance). Must be same length as `x`.
#' @note Dependency: `get_stat_t()`, `get_stat_s()`, `get_labels()` functions
ggxbar_cat = function(x,y, xlab = "Subgroups", ylab = "Average", subtitle = ""){
  
  # Testing values 
  # x = dat$bus_manufacturer
  # y = dat$price_per_seat; 
  # xlab = "Bus Manufacturer"; 
  # ylab = "Price Per Seat ($)"
  # subtitle = "Average Price Per Seat by Bus Manufacturer Across All States"

  data = tibble(x = x, y = y)

  num = x %>% numgroup()
  label = group_label(x, num) %>% unique()

  data = data %>% reframe(
    x = num,
    y = y
  )
  
  # Get statistics for each subgroup
  stat_s = get_stat_s(x = data$x, y = data$y)

  # Generate labels
  labels = stat_s %>%
    reframe(
      x = c(max(x), max(x), max(x)),
      type = c("xbbar", "upper", "lower"),
      name = c("xbbar", "+3 s", "-3 s"),
      value = c(mean(xbbar), max(upper), min(lower))
    ) %>%
    mutate(value = round(value, 2)) %>%
    mutate(text = paste0(name, " = ", value))
  
  # Get overall statistics
  stat_t = get_stat_t(x = x, y = y)

  # merge the labels back into stat_s
  stat_s = stat_s %>% 
    left_join(c("x" = "number"), y = label)

  gg = ggplot() +
    geom_hline(data = stat_t, mapping = aes(yintercept = xbbar), color = "lightgrey") +
    geom_ribbon(
      data = stat_s, 
      mapping = aes(x = x, ymin = lower, ymax = upper),
      fill = "steelblue", alpha = 0.2) +
    geom_point(
      data = stat_s,
      mapping = aes(x = x, y = xbar), size = 5
    ) +
    labs(y = ylab, subtitle = subtitle)+
    theme_classic() + 
    scale_x_continuous(
      name = xlab, 
      breaks = stat_s$x, 
      labels = str_wrap(stat_s$name, width = 15)
    ) +
    coord_flip()

  return(gg)
}

# %% bootstrapping functions #####

#' a function to create a weighted sample of the data for one type of bus
#' @param dataset tibble, the bus dataset
#' @param type string, "Type A", "Type C" or "Type D"
#' @param manuf string, the manufacturer of interest, NA if just sampling from original dataset
#' @param weight numeric, the desired percentage of the manufacturer listed as a decimal (eg 0.1 for 10%)
#' @import dplyr
getSamp = function(dataset, type, manuf = NA, weight = NA){

  # test values
  # dataset = dat
  # type = "Type A"
  # manuf = "Blue Bird"
  # weight = 0.3

  if(is.na(weight)){
    # trim data to only include type of interest
    data = dataset[dataset$bus_type == type, ]
    sample_size = dim(data)[1]
    
    output = data %>% sample_n(size = sample_size, replace = TRUE)

  } else{
    # trim data to only include type of interest
    data = dataset[dataset$bus_type == type, ]
    sample_size = dim(data)[1]
    
    # create separate tibbles with and without the manufacturer of interest
    data_manuf = data[data$bus_manufacturer == manuf, ]
    data_manuf = data_manuf[!is.na(data_manuf$price_per_seat), ]
    data_no_manuf = data[data$bus_manufacturer != manuf, ]
    data_no_manuf = data_no_manuf[!is.na(data_no_manuf$price_per_seat), ]

    # create the sample of interest
    a = round(weight*sample_size)
    b = sample_size - a

    if (a > 0){
      outputa = data_manuf %>% sample_n(size = a, replace = TRUE)
      if (b > 0){
        outputb = data_no_manuf %>% sample_n(size = b, replace = TRUE)
        output = bind_rows(outputa, outputb)
      } else{
        output = outputa
      }
    } else if (b > 0){
      outputb = data_no_manuf %>% sample_n(size = b, replace = TRUE)
      output = outputb
    } 

  }

  return(output)

}

#' a function to create bootstrapped samples
#' @param dat tibble, the bus dataset
#' @param boot_size int, the number of repetitions
#' @param type string, "Type A", "Type C" or "Type D"
#' @param manuf string, the manufacturer of interest
#' @param weights numeric vector, the desired percentages of the manufacturer listed as a decimal (eg 0.1 for 10%)
#'    weights = NA if just sampling from original dataset
getBoot = function(dat, boot_size, type, manuf, weights = NA){

  # test values
  # boot_size = 2
  # type = "Type A"
  # manuf = "Lightning eMotors/Collins Bus"
  # weights = c(0.2, 0.8)

  # create a tibble
  boot = tibble(
    w = weights %>% rep(times = boot_size),
    r = rep(1:boot_size, each = length(weights))
  ) %>% 
    group_by(w, r) %>% # for each type, weight, and rep
    reframe(
      sample = getSamp(dat, type, manuf, w) # get sample the original dataset
    ) %>% unnest(sample)

  # calculate overall statistics for each weight and rep
  boot_stat = boot %>% ungroup() %>% group_by(w, r) %>% 
    reframe(
      percentage = 100*length(which(bus_manufacturer == manuf)) / length(bus_manufacturer),
      average_pps = price_per_seat %>% mean()
    )

  return(boot_stat)
}



getScatter = function(dat, boot_size, type, manuf, weights){

  b = getBoot(dat, boot_size, type, manuf, weights)

  fit = b %>% lm(formula = average_pps ~ percentage)
  eq <- substitute(
    italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,
    list(
      a = format(as.numeric(coef(fit)[1]), digits = 2),
      b = format(as.numeric(coef(fit)[2]), digits = 2),
      r2 = format(as.numeric(summary(fit)$r.squared), digits = 3)
    )
  )

  g = b %>% ggplot(mapping = aes(x = percentage, y = average_pps))+
    geom_point() +
    labs(x = paste("Percentage of", manuf, "Buses"), 
        y = "Average Price per Seat ($/seat)",
        subtitle = str_wrap(paste("Average Price Per Seat vs Percentage Of", manuf, "for", type, "Buses" ))
    )  +
    theme_classic() +
    geom_smooth(method = "lm", se = FALSE) + # se = FALSE removes extra stuff 
    geom_text(
      x = Inf, y = Inf,
      label = as.character(as.expression(eq)),
      hjust = 1.1, vjust = 1,
      parse = TRUE
    )

  return(g)
}

# %% single anova function #####

library(dplyr)
library(ggplot2)
library(car)

run_single_anova <- function(df, response_col = "base_price", factor_col, response_lab = response_col, factor_lab = factor_col, alpha = 0.05) {
  
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
      title = paste(response_lab, "by", factor_lab),
      subtitle = paste("ANOVA p =", round(p_val, 4)),
      x = factor_lab,
      y = response_lab
    ) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1),
      plot.margin = margin(t = 15, r = 120, b = 25, l = 15)
    )
  
  return(p)
}

# %% grouped anova function #####
run_grouped_anova <- function(df, group_by, factor_col, response_col = "base_price", alpha = 0.05) {
  
  library(dplyr)
  library(car)
  
  # --- Input checks --------------------------------------------------
  if (!all(c(group_by, factor_col, response_col) %in% names(df))) {
    stop("One or more specified columns not found in dataset.")
  }
  
  df[[group_by]] <- as.factor(df[[group_by]])
  df[[factor_col]] <- as.factor(df[[factor_col]])
  
  grouped_data <- split(df, df[[group_by]])
  
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
  
  for (g in names(grouped_data)) {
    sub_df <- grouped_data[[g]]
    
    # Skip small groups or single-level factors
    if (nrow(sub_df) < 5 || nlevels(sub_df[[factor_col]]) < 3) {
      next
    }
    
    formula <- as.formula(paste("`", response_col, "` ~ `", factor_col, "`", sep = ""))
    
    model <- tryCatch(aov(formula, data = sub_df), error = function(e) NULL)
    if (is.null(model)) next
    
    summary_out <- tryCatch(summary(model)[[1]], error = function(e) NULL)
    if (is.null(summary_out) || !"Pr(>F)" %in% names(summary_out)) next
    
    # Extract safe statistics
    p_val <- as.numeric(summary_out[1, "Pr(>F)"])
    f_val <- as.numeric(summary_out[1, "F value"])
    df_factor <- as.numeric(summary_out[1, "Df"])
    df_res <- as.numeric(summary_out["Residuals", "Df"])
    
    shapiro_p <- tryCatch(shapiro.test(residuals(model))$p.value, error = function(e) NA)
    levene_p <- tryCatch(leveneTest(formula, data = sub_df)[["Pr(>F)"]][1], error = function(e) NA)
    
    results <- rbind(results, data.frame(
      Group = g,
      DF_Factor = df_factor,
      DF_Residuals = df_res,
      F_value = f_val,
      P_value = p_val,
      Shapiro_p = shapiro_p,
      Levene_p = levene_p,
      Significant = !is.na(p_val) && p_val < alpha,
      stringsAsFactors = FALSE
    ))
  }
  
  results <- results %>%
    arrange(P_value) %>%
    mutate(Significance = ifelse(Significant, "✅ Significant", "❌ Not significant"))
  
  print(results)
  invisible(results)
}