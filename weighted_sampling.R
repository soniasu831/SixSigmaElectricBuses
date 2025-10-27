# %% load in libraries and source data #####
library(dplyr) # data wrangling
library(readr) # reading csv file
library(janitor) # easy crosstabulation

# data source
source = "buses.csv" 
dat = source %>% read_csv()

source("prelim_results_SPC.R")

# convert from excel serial dates if date is available
dat = dat %>% mutate(
  date_published_or_updated = date_published_or_updated %>% as.numeric() %>% as.Date(origin = "1899-12-30")
  )

# %% getSamp function #####

#' a function to create weighted samples of data
#' @param data a tibble containing the dataset
#' @param sample_size desired number of observations in output sample
#' @param rows a vector containing the row numbers of rows to duplicate
#' @param num_duplicates a vector containing the number of duplicate entries for each row in `rows`
#' @import dplyr
getSamp = function(data, sample_size, rows, num_duplicates){

  # test values
  # data = dat[which(dat$state == "WV"), ] # only the data from WV
  # sample_size = dim(data)[1] # keep total number of buses the same
  # rows = c(3,4,5,18,19,20)
  # num_duplicates = rep(2, 6)

  # add in the duplicate rows
  rows_with_dup = rep(rows, times = num_duplicates)
  weighted_data = bind_rows(data, data[rows_with_dup, ])

  # create the sample of interest
  output = weighted_data %>% sample_n(size = sample_size, replace = TRUE)

  return(output)

}

# %% example code #####

data = dat[which(dat$state == "WV"), ] # only the data from WV
sample_size = dim(data)[1] # keep total number of buses the same
rows = c(3,4,5,18,19,20)
num_duplicates = rep(2, 6)

tabyl(data$bus_manufacturer)
#  data$bus_manufacturer  n    percent valid_percent
#              Blue Bird 10 0.34482759    0.35714286
#             GreenPower  3 0.10344828    0.10714286
#                 IC Bus  2 0.06896552    0.07142857
#             Micro Bird  1 0.03448276    0.03571429
#     Thomas Built Buses 12 0.41379310    0.42857143
#                   <NA>  1 0.03448276            NA

output = getSamp(data, sample_size, rows, num_duplicates)

tabyl(output$bus_manufacturer)
#  output$bus_manufacturer  n    percent valid_percent
#                Blue Bird  6 0.20689655    0.21428571
#               GreenPower  1 0.03448276    0.03571429
#                   IC Bus  2 0.06896552    0.07142857
#               Micro Bird  1 0.03448276    0.03571429
#       Thomas Built Buses 18 0.62068966    0.64285714
#                     <NA>  1 0.03448276            NA



# %%

data = dat[which(dat$state == "WA"), ] # only the data from WA
# add a column that is per seat 
data = data %>% mutate(
  price_per_seat = base_price / seating_capacity
) 
sample_size = dim(data)[1] # keep total number of buses the same
rows = c(12, 15, 21, 24, 26, 26)

output1 = getSamp(data, sample_size, rows, rep(1, 6))
output2 = getSamp(data, sample_size, rows, rep(2, 6))

tabyl(data$bus_manufacturer) # BlueBird  22%
tabyl(output1$bus_manufacturer) # BlueBird 33%
tabyl(output2$bus_manufacturer) # BlueBird 63%





# %%
g1 = bus_spc_avg(data$bus_manufacturer, data$price_per_seat, 
            xlab = "Manufacturer", ylab = "Bus Price per Seat ($/seat)", 
            label_width = 60, label_vjust = 2.75)

g2 = bus_spc_avg(output1$bus_manufacturer, output1$price_per_seat, 
            xlab = "Manufacturer", ylab = "Bus Price per Seat ($/seat)", 
            label_width = 60, label_vjust = 4)

g1

g2


# %%
percent = c()
average_price = c()
for (i in c(1:100)){
  output = getSamp(data, sample_size, rows, rep(1, 6))
  average_price[i] = get_stat_t(output$bus_manufacturer, output$price_per_seat)$xbbar
  percent[i] = 100*length(which(output$bus_manufacturer == "Blue Bird")) / length(output$bus_manufacturer)
}
for (i in c(101:200)){
  output = getSamp(data, sample_size, rows, rep(2, 6))
  average_price[i] = get_stat_t(output$bus_manufacturer, output$price_per_seat)$xbbar
  percent[i] = 100*length(which(output$bus_manufacturer == "Blue Bird")) / length(output$bus_manufacturer)
}
for (i in c(201:300)){
  output = getSamp(data, sample_size, rows, rep(3, 6))
  average_price[i] = get_stat_t(output$bus_manufacturer, output$price_per_seat)$xbbar
  percent[i] = 100*length(which(output$bus_manufacturer == "Blue Bird")) / length(output$bus_manufacturer)
}
    
ggplot()+
  geom_point(mapping = aes(x = percent, y = average_price), size = 1) + 
  labs(x = "percentage of BlueBird", y = "average price per seat in WA") 

percentage_vs_price = tibble(
  percent = percent,
  average_price = average_price
)

ggprocess(x = percent, y = average_price, xlab = "Percentage of Blue Bird in WA", ylab = "Average Bus Price per Seat in WA ($/seat)")
