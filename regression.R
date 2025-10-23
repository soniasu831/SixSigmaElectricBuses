# %% load packages and data #####

# load in libraries
library(tidyverse) # data wrangling
library(broom) # regression
library(readr) # reading csv file

# load in data
source = "buses.csv" 
dat = source %>% read_csv()

# convert from excel serial dates if date is available #####
dat = dat %>% mutate(
  date_published_or_updated = date_published_or_updated %>% as.numeric() %>% as.Date(origin = "1899-12-30")
  )

# numerical categories: date_published_or_updated, seating_capacity

# %% seating capacity #####


dat %>% lm(formula = base_price ~ seating_capacity) %>% summary()

# price_preducted = 237471 + seating_capacity_obs * 1869

# Call:
# lm(formula = base_price ~ seating_capacity, data = .)

# Residuals:
#    Min     1Q Median     3Q    Max 
# -98540 -23540  -3884  21057 138208 

# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      237471.0     8524.6   27.86   <2e-16 ***
# seating_capacity   1869.0      136.9   13.65   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 39030 on 162 degrees of freedom
#   (13 observations deleted due to missingness)
# Multiple R-squared:  0.5349,	Adjusted R-squared:  0.532 
# F-statistic: 186.3 on 1 and 162 DF,  p-value: < 2.2e-16

dat %>%
  ggplot(mapping = aes(x = seating_capacity, y = base_price)) +
  geom_point(size = 3, shape = 21, 
             fill = "white", color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE) + # se = FALSE removes extra stuff
  theme_classic()

# there is an error when this plots because there are 13 instances where the seating_capacity is NA, so it obviously can't plot the NAs

# sum(dat$seating_capacity %>% is.na())
# [1] 9



# %%

dat %>% lm(formula = base_price ~ purchase_year) %>% summary()

# price_preducted = 3.538e+05 + purchase_year_obs * -2.798e-01

# Call:
# lm(formula = base_price ~ purchase_year, data = .)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -153452  -39738   14285   38518  173021 

# Coefficients:
#                             Estimate Std. Error t value Pr(>|t|)
# (Intercept)                3.538e+05  2.321e+05   1.525    0.129
# date_published_or_updated -2.798e-01  1.217e+01  -0.023    0.982

# Residual standard error: 57620 on 166 degrees of freedom
#   (9 observations deleted due to missingness)
# Multiple R-squared:  3.181e-06,	Adjusted R-squared:  -0.006021 
# F-statistic: 0.000528 on 1 and 166 DF,  p-value: 0.9817

dat %>%
  ggplot(mapping = aes(x = purchase_year, y = base_price)) +
  geom_point(size = 3, shape = 21, 
             fill = "white", color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE) + # se = FALSE removes extra stuff
  theme_classic()


# %%

install.packages("gmodels")
library(gmodels)

CrossTable(dat$bus_manufacturer)
