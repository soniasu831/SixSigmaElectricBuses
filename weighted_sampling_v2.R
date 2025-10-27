# %% load in libraries and source data #####
library(dplyr) # data wrangling
library(readr) # reading csv file
library(tidyr) # more data wrangling
library(janitor) # easy crosstabs

# data source
source = "buses.csv" 
dat = source %>% read_csv()

source("tim_functions_process_control.R")

dat = dat %>% mutate(
  # convert from excel serial dates if date is available
  date_published_or_updated = date_published_or_updated %>% as.numeric() %>% as.Date(origin = "1899-12-30"),
  price_per_seat = round(base_price / seating_capacity, 2)
  )

# set states of interest for this analysis
states = dat$state %>% unique()
states = states[!states %in% c("ME", "OH", "SC", "VA")] # remove states where there is no Blue Bird

# %% getSamp function #####

#' a function to create a weighted sample of the data for a state
#' @param dataset tibble, the bus dataset
#' @param state string, the state of interest as a two-letter code
#' @param manuf string, the manufacturer of interest, NA if just sampling from original dataset
#' @param weight numeric, the desired percentage of the manufacturer listed as a decimal (eg 0.1 for 10%)
#' @import dplyr
getSamp = function(dataset, state, manuf = NA, weight = NA){

  # test values
  # dataset = dat
  # state = "WA"
  # manuf = "Blue Bird"
  # weight = 0

  if(is.na(weight)){
    data = dataset[dat$state == state, ]
    sample_size = dim(data)[1]
    
    output = dataset %>% sample_n(size = sample_size, replace = TRUE)

  } else{
    # trim data to only include state of interest
    data = dataset[dat$state == state, ]
    sample_size = dim(data)[1]
    
    # create separate tibbles with and without the manufacturer of interest
    data_manuf = data[data$bus_manufacturer == manuf, ]
    data_no_manuf = data[data$bus_manufacturer != manuf, ]

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

# %% getBoot function #####
#' a function to create bootstrapped samples
#' @param dat tibble, the bus dataset
#' @param boot_size int, the number of repetitions
#' @param states string vector, the state of interest as a two-letter code
#' @param manuf string vector, the manufacturer of interest
#' @param weights numeric vector, the desired percentages of the manufacturer listed as a decimal (eg 0.1 for 10%)
#'    weights = NA if just sampling from original dataset
getBoot = function(dat, boot_size, states, manuf, weights = NA){

  # create a tibble
  boot = tibble(
    s = states %>% rep(each = length(weights)*boot_size),
    w = weights %>% rep(times = length(states)*boot_size),
    r = rep(1:boot_size, each = length(weights)) %>% rep(times = length(states)),
  ) %>% 
    group_by(s, w, r) %>% # for each state, weight, and rep
    reframe(
      sample = getSamp(dat, s, manuf, w) # get sample the original dataset
    ) %>% unnest(sample)

  # remove NAs from the dataset
  boot <- boot[!is.na(boot$price_per_seat), ]

  # calculate overall statistics for each weight and rep
  boot_stat = boot %>% ungroup() %>% group_by(w, r) %>% 
    reframe(
      percentage = 100*length(which(bus_manufacturer == manuf)) / length(bus_manufacturer),
      average_pps = get_stat_t(bus_manufacturer, price_per_seat)$xbbar
    )

  return(boot_stat)
}

# %% scatter plot with trendline

b1 = getBoot(dat, boot_size = 10, states, "Blue Bird", seq(from = .05, to = .95, by = 0.05))

b1 %>% ggplot(mapping = aes(x = percentage, y = average_pps))+
  geom_point() +
  labs(x = "Percentage of Blue Bird", 
       y = "Average Bus Price per Seat ($/seat)",
       subtitle = "Comparison of Average Price Per Seat vs Percentage Of Blue Bird" ) +
  theme_classic() +
  geom_smooth(method = "lm", se = FALSE) # se = FALSE removes extra stuff

# b1 %>% lm(formula = average_pps ~ percentage) %>% summary()


# %% simulate data and create box plots

b2 = getBoot(dat, boot_size = 20, states, "Blue Bird", seq(from = .1, to = .9, by = 0.2))

b2 %>% ggplot()+
  geom_boxplot(mapping = aes(x = w, y = average_pps, group = w)) +
  labs(x = "Percentage of Blue Bird", 
       y = "Average Bus Price per Seat ($/seat)",
       subtitle = "Comparison of Average Price Per Seat vs Percentage Of Blue Bird" ) +
  theme_classic()


# %% calculate confidence intervals 

# bootstrap original dataset
b_nom = getBoot(dat, boot_size = 100, states, "Blue Bird")
b_x = getBoot(dat, boot_size = 100, states, "Blue Bird", weights = c(0.6))

alpha = 0.95

ci_price = get_stat_t(b_nom$r, b_nom$average_pps) %>% reframe(
  percent = get_stat_t(b_nom$r, b_nom$percentage)$xbbar,
  z = qnorm(alpha),
  xbbar = xbbar,
  upper = xbbar + z*sigma_t,
  lower = xbbar - z*sigma_t
)

ci_price_x = get_stat_t(b_x$r, b_x$average_pps) %>% reframe(
  percent = get_stat_t(b_x$r, b_x$percentage)$xbbar,
  z = qnorm(alpha),
  xbbar = xbbar,
  upper = xbbar + z*sigma_t,
  lower = xbbar - z*sigma_t
)


bind_rows(ci_price, ci_price_x)

# at 90% CI, there is quite a bit of overlap between the two, 
# although the mean price per seat at 60% Blue Bird is lower than nominal
# # A tibble: 2 × 5
#   percent     z xbbar upper lower
#     <dbl> <dbl> <dbl> <dbl> <dbl>
# 1    28.0  1.64 9006. 9853. 8160.
# 2    60.1  1.64 8801. 9887. 7715.

# %%

dat %>% tabyl(state, bus_manufacturer)
# no blue bird in ME, OH, SC, VA
# SC has NAs for seating