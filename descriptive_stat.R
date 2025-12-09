# %% load in libraries and source data #####
library(dplyr) # data wrangling
library(readr) # reading csv file

# data source
source = "buses_with_price_per_seat_and_converted_dates.csv" 
dat = source %>% read_csv()

dat_A = dat %>% filter(bus_type == "Type A")
dat_C = dat %>% filter(bus_type == "Type C")
dat_D = dat %>% filter(bus_type == "Type D")

# %%

get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# %%

options(pillar.sigfig = 6)

dat_A %>% reframe(
  n = n(),
  min = min(base_price),
  max = max(base_price),
  mean = mean(base_price),
  median = median(base_price),
  mode = get_mode(base_price),
  sd = sd(base_price)
)

dat_C %>% reframe(
  n = n(),
  min = min(base_price),
  max = max(base_price),
  mean = mean(base_price),
  median = median(base_price),
  mode = get_mode(base_price),
  sd = sd(base_price)
)

dat_D %>% reframe(
  n = n(),
  min = min(base_price),
  max = max(base_price),
  mean = mean(base_price),
  median = median(base_price),
  mode = get_mode(base_price),
  sd = sd(base_price)
)
