# --- Bivariate regression summary for bus pricing ---------------------------
# What it does:
# 1) Installs/loads needed packages
# 2) Reads your CSV
# 3) Cleans a few common categorical fields
# 4) Finds all numeric predictors (excluding Purchase.Price)
# 5) Fits lm(Purchase.Price ~ X) for each numeric X
# 6) Writes a ranked summary CSV: variable, n, intercept, slope, R2, F, p-values, CIs
# 7) (Optional) Repeats #5 grouped by Bus.Type and writes a second CSV
 
# ===== 0) Packages =====
need <- c("tidyverse", "broom", "readr", "stringr")
to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(need, library, character.only = TRUE))
 
# ===== 1) Read data (edit the path if needed) =====
# If your file is named exactly like this, leave as-is. Otherwise change it.
buses <- read.csv("buses (1).csv", stringsAsFactors = FALSE)
 
# ===== 2) Basic cleaning for typical fields (safe if columns exist) =====
clean_if <- function(df, col, fun) {
  if (col %in% names(df)) df[[col]] <- fun(df[[col]])
  df
}
buses <- buses %>%
  clean_if("Bus.Type", ~ str_to_title(trimws(as.character(.)))) %>%
  clean_if("Bus.Manufacturer", ~ str_to_title(trimws(as.character(.)))) %>%
  clean_if("Bus.Model", ~ str_to_title(trimws(as.character(.)))) %>%
  clean_if("State", ~ str_to_upper(trimws(as.character(.))))
 
# Convert obvious categoricals to factors if present
factor_cols <- c("Bus.Manufacturer","Bus.Model","Bus.Type","State","Vehicle.Dealer",
                 "Source.Type","Special.Needs.Bus")
for (cc in factor_cols) {
  if (cc %in% names(buses)) buses[[cc]] <- as.factor(buses[[cc]])
}
 
# ===== 3) Validate target column =====
target <- "Purchase.Price"
stopifnot(target %in% names(buses))
 
# Remove missing or non-positive prices (adjust logic if needed)
buses <- buses %>% filter(!is.na(.data[[target]]), .data[[target]] > 0)
 
# ===== 4) Identify numeric predictors (exclude target) =====
num_cols <- names(buses)[vapply(buses, is.numeric, logical(1))]
preds <- setdiff(num_cols, target)
 
if (length(preds) == 0) {
  stop("No numeric predictors found besides Purchase.Price. Check your data types.")
}
 
# ===== 5) Fit bivariate regressions for each numeric predictor =====
# Helper: safe CI extraction for the slope term
extract_slope_ci <- function(model, term_name) {
  # Try to get CI; return NA if not available
  ci <- tryCatch(confint(model), error = function(e) NULL)
  if (is.null(ci) || !(term_name %in% rownames(ci))) {
	return(c(NA_real_, NA_real_))
  }
  as.numeric(ci[term_name, ])
}
 
summaries <- purrr::map_dfr(preds, function(x) {
  # Build formula: Purchase.Price ~ x
  f <- as.formula(paste(target, "~", x))
 
  # Drop rows with NA in either target or predictor
  d <- buses %>% select(all_of(c(target, x))) %>% drop_na()
  n_obs <- nrow(d)
  if (n_obs < 3) {
	return(tibble(
  	variable = x, n = n_obs,
  	intercept = NA_real_, slope = NA_real_,
  	r.squared = NA_real_, adj.r.squared = NA_real_,
  	f_stat = NA_real_, model_p = NA_real_,
  	slope_p = NA_real_, slope_ci_lo = NA_real_, slope_ci_hi = NA_real_
	))
  }
 
  m <- lm(f, data = d)
  gl <- broom::glance(m)
  td <- broom::tidy(m)
 
  # Get slope row
  slope_row <- td %>% filter(term == x)
  slope <- slope_row$estimate %||% NA_real_
  slope_p <- slope_row$p.value %||% NA_real_
 
  # Intercept
  intercept <- td %>% filter(term == "(Intercept)") %>% pull(estimate) %||% NA_real_
 
  # CI for slope
  ci <- extract_slope_ci(m, x)
 
  tibble(
	variable = x,
	n = n_obs,
	intercept = intercept,
	slope = slope,
	r.squared = gl$r.squared %||% NA_real_,
	adj.r.squared = gl$adj.r.squared %||% NA_real_,
	f_stat = gl$statistic %||% NA_real_,
	model_p = gl$p.value %||% NA_real_,
	slope_p = slope_p,
	slope_ci_lo = ci[1],
	slope_ci_hi = ci[2]
  )
}) %>%
  arrange(desc(r.squared))
 
# ===== 6) Write summary to CSV =====
readr::write_csv(summaries, "bivariate_regression_summary.csv")
 
# ===== 7) (Optional) Grouped bivariate regressions by Bus.Type =====
if ("Bus.Type" %in% names(buses)) {
  grouped_summaries <- buses %>%
    drop_na(.data[[target]], Bus.Type) %>%
	group_by(Bus.Type) %>%
	group_map(~{
  	df <- .x
  	preds_local <- setdiff(names(df)[vapply(df, is.numeric, logical(1))], target)
  	if (length(preds_local) == 0) return(tibble())
  	
      purrr::map_dfr(preds_local, function(x) {
    	dd <- df %>% select(all_of(c(target, x))) %>% drop_na()
    	n_obs <- nrow(dd)
    	if (n_obs < 3) {
          return(tibble(
        	Bus.Type = first(df$Bus.Type), variable = x, n = n_obs,
        	intercept = NA_real_, slope = NA_real_,
        	r.squared = NA_real_, adj.r.squared = NA_real_,
        	f_stat = NA_real_, model_p = NA_real_,
        	slope_p = NA_real_, slope_ci_lo = NA_real_, slope_ci_hi = NA_real_
      	))
    	}
    	m <- lm(as.formula(paste(target, "~", x)), data = dd)
    	gl <- glance(m)
    	td <- tidy(m)
    	slope_row <- td %>% filter(term == x)
    	slope <- slope_row$estimate %||% NA_real_
    	slope_p <- slope_row$p.value %||% NA_real_
    	intercept <- td %>% filter(term == "(Intercept)") %>% pull(estimate) %||% NA_real_
    	ci <- tryCatch(confint(m), error = function(e) NULL)
    	if (!is.null(ci) && x %in% rownames(ci)) {
      	ci_lo <- ci[x, 1]; ci_hi <- ci[x, 2]
    	} else {
      	ci_lo <- NA_real_; ci_hi <- NA_real_
    	}
    	tibble(
      	Bus.Type = first(df$Bus.Type),
      	variable = x,
      	n = n_obs,
      	intercept = intercept,
      	slope = slope,
      	r.squared = gl$r.squared %||% NA_real_,
          adj.r.squared = gl$adj.r.squared %||% NA_real_,
      	f_stat = gl$statistic %||% NA_real_,
      	model_p = gl$p.value %||% NA_real_,
      	slope_p = slope_p,
      	slope_ci_lo = ci_lo,
      	slope_ci_hi = ci_hi
    	)
  	}) %>% arrange(desc(r.squared))
	}) %>% bind_rows()
 
  readr::write_csv(grouped_summaries, "bivariate_by_BusType_summary.csv")
}
 
# ===== 8) Console preview =====
message("Top bivariate predictors by R^2:")
print(summaries %>% select(variable, n, r.squared, slope, slope_p) %>% head(10))
 
message('Files written: "bivariate_regression_summary.csv"',
    	if ("Bus.Type" %in% names(buses)) ' and "bivariate_by_BusType_summary.csv"' else "")