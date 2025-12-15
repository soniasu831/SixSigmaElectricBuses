# %% load in libraries and data #####

library(dplyr) # data wrangling
library(readr) # reading csv file
library(ggplot2) # plotting 
library(ggpubr) # plot stacking
library(stringr) # string wrapping

# load in Tim's process control functions #####
# edits to Tim's functions: 
# change line 98 to: g1 = ggplot() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
# change line 168 to: sigma_s = sqrt(mean(s^2, na.rm = TRUE))
# change line 576 to: hjust = -0.25 # horizontally justify the labels

# https://github.com/timothyfraser/sysen/blob/main/functions/functions_process_control.R
source("tim_functions_process_control.R")

# load in spc_tests from hackathon #####
# to account for variable control limits replace the code for 
# test 6 and 7 with the code below: 
  # # test 6 #################

  # # evaluate whether each point is within 1s
  # within_1s = c()
  # for (i in 1:length(averages)){
  #   point = averages[i]
  #   point_s = sigma[i]
  #   # if the point is above the centerline
  #   if (point > xbbar){
  #     # if the point is within 1s
  #     if (point < (xbbar + point_s)){ 
  #       within_1s = append(within_1s, TRUE)
  #     } else{
  #       within_1s = append(within_1s, FALSE)
  #     }
  #   } else{ # below the centerline
  #     # if the point is within 1s
  #     if (point > (xbbar - point_s)){ 
  #       within_1s = append(within_1s, TRUE)
  #     } else{
  #       within_1s = append(within_1s, FALSE)
  #     }
  #   }
  # }
    
  # # count the number of times there are at least 15 values within a window 
  # # of length 15 that are within 1s of the centerline
  # test6 = sum(window(within_1s, 15) >= 15)

  # # update output vector
  # if(!is.na(test6)){
  #   if (sum(test6) == 0){
  #     output$test6 = TRUE
  #   } else if (sum(test6) >= 1){
  #     output$test6 = FALSE
  #   }
  # }

  # # test 7 #################

  # # evaluate whether each point is outside 1s
  # outside_1s = c()
  # for (i in 1:length(averages)){
  #   point = averages[i]
  #   point_s = sigma[i]
  #   # if the point is above the centerline
  #   if (point > xbbar){
  #     # if the point is outside 1s
  #     if (point > (xbbar + point_s)){ 
  #       outside_1s = append(outside_1s, TRUE)
  #     } else{
  #       outside_1s = append(outside_1s, FALSE)
  #     }
  #   } else{ # below the centerline
  #     # if the point is outside 1s
  #     if (point < (xbbar - point_s)){ 
  #       outside_1s = append(outside_1s, TRUE)
  #     } else{
  #       outside_1s = append(outside_1s, FALSE)
  #     }
  #   }
  # }

  # # count the number of times there are at least 8 values within a window 
  # # of length 8 that are within 1s of the centerline
  # test7 = sum(window(outside_1s, 8) >= 8)

  # # update output vector
  # if(!is.na(test7)){
  #   if (sum(test7) == 0){
  #     output$test7 = TRUE
  #   } else if (sum(test7) >= 1){
  #     output$test7 = FALSE
  #   }
  # }

#######################################################

# https://github.com/soniasu831/team18minussiva/blob/main/package/R/spc_tests.R
source("spc_tests.R")

# data source #####
source = "buses.csv" 
dat = source %>% read_csv()

# convert from excel serial dates if date is available #####
dat = dat %>% mutate(
  date_published_or_updated = date_published_or_updated %>% as.numeric() %>% as.Date(origin = "1899-12-30")
  )

##### HELPER FUNCTIONS #####

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

#' helper function to create legend for when categorical groups need
#' to be represented as numerical groups
#' @param name a vector, the original categorical grouping data
#' @param number the output of numgroup()
#' @import dplyr
#' @import stringr
group_label = function(name, number) {
  label = tibble(
    name = name,
    number = number
  ) %>% reframe(
    label = paste(number, ": ", name, ", ", sep = "") %>% unique()
  )

  output = label$label %>% paste(collapse = " ") %>% str_sub(end = -3)
  return(output)
}

#' helper function to make spc average charts the way Louise likes
#' uses functions from Tim with edits as noted in the top of this doc
#' @param x
#' @param y
#' @param xlab
#' @param ylab
#' @param label_width
#' @param label_vjust
#' @param numeric are your groups numbers?
#' @import dplyr
#' @import stringr
#' @note Dependency: `group_label()`, `numgroup()`
#'   https://github.com/timothyfraser/sysen/blob/main/functions/functions_process_control.R
bus_spc_avg = function(x, y, xlab, ylab, label_width, label_vjust, numeric = FALSE){
  
  if(numeric){
    num = x
    label = ""
  } else{
    num = x %>% numgroup
    label = group_label(x, num)
  }

  ggxbar(x = num, y = y, xlab = xlab, ylab = ylab) + 
  theme_classic() + 
  annotate("text", x = mean(num %>% unique()), y = min(y),
           label = str_wrap(label, width = label_width), 
           vjust = label_vjust, size = 3) +
  theme(plot.margin = unit(c(0.5,3,2,0.25), "cm")) + # adjust margins
  coord_cartesian(clip = "off") # turn off clipping
}

#' helper function to run spc test son bus data
#' uses functions from Tim with edits as noted in the top of this doc
#' uses spc_tests.R from hackathon
#' @param x
#' @param y
#' @param numeric are your groups numbers?
#' @import dplyr
#' @note Dependency: `group_label()`, `numgroup()`
#'   https://github.com/timothyfraser/sysen/blob/main/functions/functions_process_control.R
#'   https://github.com/soniasu831/team18minussiva/blob/main/package/R/spc_tests.R
bus_spc_test_avg = function(x, y, numeric = FALSE){
  
  if(numeric){
    num = x
  } else{
    num = x %>% numgroup
  }

  stat_s = get_stat_s(x, y)
  output = spcTest(stat_s$xbar, stat_s$se)

  return(output)
}


# %% process overview diagrams #####

# price vs state
ggprocess(x = dat$state, y = dat$base_price, xlab = "State", ylab = "Bus Price ($)")

# price vs model
ggprocess(x = dat$bus_model, y = dat$base_price, xlab = "Bus Model", ylab = "Bus Price ($)")

# price vs manufacturer
ggprocess(x = dat$bus_manufacturer, y = dat$base_price, xlab = "Bus Manufacturer", ylab = "Bus Price ($)")

# price vs bus type
ggprocess(x = dat$bus_type, y = dat$base_price, xlab = "Bus Type", ylab = "Bus Price ($)")

# price vs dealer
ggprocess(x = dat$vehicle_dealer, y = dat$base_price, xlab = "Dealer", ylab = "Bus Price ($)")


# %% spc charts + tests #####

# UCL/LCL lines change since subgroups are different sizes
# se = sigma_s / sqrt(n) where n is the number of samples in the subgroup

# spc tests have limited applicability here since several of the "xxx in a row 
# above/below xxx" tests are looking for early indicators of special cause and our 
# groups are generally independent
# it's still nice as a little flag of "hey! you might want to look here"

# price vs state
bus_spc_avg(dat$state, dat$base_price, 
            xlab = "State", ylab = "Bus Price ($)", 
            label_width = 80, label_vjust = 3.5)

bus_spc_test_avg(dat$state, dat$base_price)
# all tests passed
# generally stable pattern of pricing across states
# prices higher in Utah - worth taking a closer look at

# price vs model
bus_spc_avg(dat$bus_model, dat$base_price, 
            xlab = "Model", ylab = "Bus Price ($)", 
            label_width = 80, label_vjust = 2.5)

bus_spc_test_avg(dat$bus_model, dat$base_price)
# test 1, 2, 3, and 5 failed
# 3, 4, 6, 7, 8 above UCL 
#   saf-t-liner c2 jouley
#   vision
#   CE
#   all-american


# price vs type
bus_spc_avg(dat$bus_type, dat$base_price, 
            xlab = "Type", ylab = "Bus Price ($)", 
            label_width = 80, label_vjust = 10)

# price vs manufacturer
bus_spc_avg(dat$bus_manufacturer, dat$base_price, 
            xlab = "Manufacturer", ylab = "Bus Price ($)", 
            label_width = 80, label_vjust = 2.75)

bus_spc_test_avg(dat$bus_manufacturer, dat$base_price)
# test 1, 2 failed. not enough data for test 6
# 2, 3, 6, and 7 are above UCL
#   thomas built buses
#   blue bird
#   greenpower
#   ic bus


dat_a = dat %>% mutate(
  price_per_seat = base_price / seating_capacity
) 

dat_a <- dat_a[!is.na(dat_a$price_per_seat), ]

# bus price per seat vs manufacturer
bus_spc_avg(dat_a$bus_manufacturer, dat_a$price_per_seat,
            xlab = "Bus Manufacturer", ylab = "Bus Price Per Seat ($/seat)", 
            label_width = 100, label_vjust = 2.75)

# price vs dealer
bus_spc_avg(dat$vehicle_dealer, dat$base_price, 
            xlab = "Dealer", ylab = "Bus Price ($)", 
            label_width = 100, label_vjust = 1)

bus_spc_test_avg(dat$vehicle_dealer, dat$base_price)


# price vs seating capacity
bus_spc_avg(dat$seating_capacity, dat$base_price, 
            xlab = "Seating Capacity", ylab = "Bus Price ($)", 
            label_width = 100, label_vjust = 1, numeric = TRUE)

# price vs date
bus_spc_avg(dat$date_published_or_updated, dat$base_price, 
            xlab = "Date", ylab = "Bus Price ($)", 
            label_width = 100, label_vjust = 1, numeric = TRUE)


# %% moving range charts #####

# price vs time MR
ggmr(dat$date_published_or_updated, dat$base_price)

ggmr(dat$date_published_or_updated[which(dat$bus_manufacturer == "Blue Bird")], 
     dat$base_price[which(dat$bus_manufacturer == "Blue Bird")])


# %% just look at Utah

dat$base_price[which(dat$state == "UT")]
dat$bus_model[which(dat$state == "UT")]
dat$bus_manufacturer[which(dat$state == "UT")]
# [1] 521459 400000
# [1] "All-American"          "Saf-T-Liner C2 Jouley"
# [1] "Blue Bird"          "Thomas Built Buses"


# %% looking just at thomas built buses

dat$state[which(dat$bus_manufacturer == "Thomas Built Buses")]
dat$base_price[which(dat$bus_manufacturer == "Thomas Built Buses")]
dat$bus_model[which(dat$bus_manufacturer == "Thomas Built Buses")]
dat$bus_manufacturer[which(dat$bus_manufacturer == "Thomas Built Buses")]

# all but one of the thomas built buses is the saf-t-liner c2 jouley

# %% looking just at blue bird

dat$state[which(dat$bus_manufacturer == "Blue Bird")]
dat$base_price[which(dat$bus_manufacturer == "Blue Bird")]
dat$bus_model[which(dat$bus_manufacturer == "Blue Bird")]
dat$bus_manufacturer[which(dat$bus_manufacturer == "Blue Bird")]

# mix of all american and vision


# %% looking just at GreenPower

dat$state[which(dat$bus_manufacturer == "GreenPower")]
dat$base_price[which(dat$bus_manufacturer == "GreenPower")]
dat$bus_model[which(dat$bus_manufacturer == "GreenPower")]
dat$bus_manufacturer[which(dat$bus_manufacturer == "GreenPower")]



# %% looking just at IC Bus

dat$state[which(dat$bus_manufacturer == "IC Bus")]
dat$base_price[which(dat$bus_manufacturer == "IC Bus")]
dat$bus_model[which(dat$bus_manufacturer == "IC Bus")]
dat$bus_manufacturer[which(dat$bus_manufacturer == "IC Bus")]
