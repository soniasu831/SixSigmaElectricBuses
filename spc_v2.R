# %% load in libraries and source data #####
library(dplyr) # data wrangling
library(readr) # reading csv file
library(tidyr) # more data wrangling
library(janitor) # easy crosstabs
library(ggpubr) # plot stacking
library(stringr) # text wrapping

# data source
# load in the edited csv
dat = read_csv("buses_with_price_per_seat_and_converted_dates.csv")

# load in tim process control functions
source("tim_functions_process_control.R")

# remove NAs from the dataset
dat = dat[!is.na(dat$price_per_seat), ]

dat_A = dat[dat$bus_type == "Type A", ]
dat_C = dat[dat$bus_type == "Type C", ]
dat_D = dat[dat$bus_type == "Type D", ]

# %%

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

# %%

g1 = ggxbar_cat(dat_A$bus_manufacturer,dat_A$price_per_seat, 
  xlab = "Bus Manufacturer", ylab = "Average", 
  subtitle = paste0("Type A (n = ", dat_A %>% nrow() %>% as.character(), ")"))

g2 = ggxbar_cat(dat_C$bus_manufacturer,dat_C$price_per_seat, 
  xlab = "Bus Manufacturer", ylab = "Average", 
  subtitle = paste0("Type C (n = ", dat_C %>% nrow() %>% as.character(), ")"))

g3 = ggxbar_cat(dat_D$bus_manufacturer,dat_D$price_per_seat, 
  xlab = "Bus Manufacturer", ylab = "Average", 
  subtitle = paste0("Type D (n = ", dat_D %>% nrow() %>% as.character(), ")"))

# gg = ggarrange(g1, g2, g3, ncol = 1, nrow = 3)

g1
g2
g3

ggsave('bus_type_a.png', plot = g1, width = 6.5, height = 4, dpi = 300)
ggsave('bus_type_c.png', plot = g2, width = 6.5, height = 4, dpi = 300)
ggsave('bus_type_d.png', plot = g3, width = 6.5, height = 4, dpi = 300)


dat_A$price_per_seat %>% mean()
dat_D$price_per_seat %>% mean()
dat_C$price_per_seat %>% mean()

# [1] 12390.65
# [1] 4966.584
# [1] 5541.17