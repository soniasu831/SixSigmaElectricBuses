#' @name spcTest
#' @title Run SPC Tests on Subgroup 
#' @description
#' This function evaluates whether a vector passes each of the standard SPC tests.
#'  Each test checks for different types of patterns such as runs below/above control 
#'  limits, trends, or oscillations.
#' @param averages Numeric. Subgroup average values.
#' @param sigma Numeric. Standard deviation of the subgroup averages.
#' @return A tibble with colums for each test. Each entry is 'TRUE' if the subgroup
#'  averages pass the test and 'FALSE' if they fail.
#' @details
#' - Test 1: one point beyond zone a
#' - Test 2: two out of three points in a row beyond zone b
#' - Test 3: four out of five points in a row beyond zone c
#' - Test 4: eight points in a row on the same side of the centerline
#' - Test 5: six points in a rox steadily increasing or decreasing
#' - Test 6: fifteen points in a row in zone c
#' - Test 7: eight points in a rox on both sides of the centerline (none in zone c)
#' - Test 8: fourteen points in a row alternating up and down
#' @import dplyr
#' @export
#' 
#' @examples
#' \dontrun{
#' averages <- c(10.1, 10.2, 9.9, 10.3, 10.0)
#' sigma <- sd(averages)
#' spcTest(averages, sigma)
#' }

spcTest = function(averages, sigma){
  
# initialize output
  output = tibble(
    test1 = NA,
    test2 = NA,
    test3 = NA,
    test4 = NA,
    test5 = NA,
    test6 = NA,
    test7 = NA,
    test8 = NA
  )
  
  #' Window helper function
  #' counts the number of 'TRUE' values within a window of specified size
  #' data, a logical vector of booleans
  #' len, Interger. the window size
  #' return a numeric vector where each element represents the number of 'TRUE' values
  #'  within each rolling window
  
  window = function(data, len){
    output = c()

    if (len < length(data)){
      # look through data in windows of length len
      for (i in 1:(length(data)-(len-1))){
        temp = sum(data[i:(i+(len-1))])
        output = append(output, temp)
      }
    }else {
      output = NA
    }

    return(output)
  }

  # test 1 #################
  
  xbbar = mean(averages)

  greater_than_3s = averages > (xbbar + sigma*3)
  less_than_3s = averages < (xbbar - sigma*3)

  # count the number of times there is 1 value greater than 3s or less than
  # 3s from the centerline
  test1 = sum(window(greater_than_3s, 1) >= 1) + sum(window(less_than_3s, 1) >= 1)

  # update output vector
  if(!is.na(test1)){
    if (sum(test1) == 0){
      output$test1 = TRUE
      } else if (sum(test1) >=1){
      output$test1 = FALSE
      }
    }
  
  # test 2 #################

  greater_than_2s = averages > (xbbar + sigma*2)
  less_than_2s = averages < (xbbar - sigma*2)

  # count the number of times there are at least 2 values within a 
  # window of length 3 that are greater than 2s or less than 2s from 
  # the centerline
  test2 = sum(window(greater_than_2s, 3) >= 2) + sum(window(less_than_2s, 3) >= 2)

  # update output vector
  if(!is.na(test2)){
    if (sum(test2) == 0){
      output$test2 = TRUE
    } else if (sum(test2) >= 1){
      output$test2 = FALSE
    }
  }

  # test 3 #################

  greater_than_1s = averages > (xbbar + sigma)
  less_than_1s = averages < (xbbar - sigma)

  # count the number of times there are at least 4 values within a window 
  # of length 5 that are greater than 1s or less than 1s from the centerline
  test3 = sum(window(greater_than_1s, 5) >= 4) + sum(window(less_than_1s, 5) >= 4)

  # update output vector
  if(!is.na(test3)){
    if (sum(test3) == 0){
      output$test3 = TRUE
    } else if (sum(test3) >= 1){
      output$test3 = FALSE
    }
  }

  # test 4 #################

  greater_than_c = averages > xbbar
  less_than_c = averages < xbbar

  # count the number of times there are at least 8 values within a window 
  # of length 8 that are greater than or less than the centerline
  test4 = sum(window(greater_than_c, 8) >= 8) + 
    sum(window(less_than_c, 8) >= 8)

  # update output vector
  if(!is.na(test4)){
    if (sum(test4) == 0){
      output$test4 = TRUE
    } else if (sum(test4) >= 1){
      output$test4 = FALSE
    }
  }

  # test 5 #################

  # evaluate whether the next point is greater than or less than the previous
  increasing = averages[-length(averages)] < averages[-1]
  decreasing = averages[-length(averages)] > averages[-1]

  # count the number of times there are at least 6 values within a window 
  # of length 6 that are increasing or decreasing
  # since increasing or decreasing is a difference, we look for >=5 
  # consequtive values
  test5 = sum(window(increasing, 5) >= 5) + sum(window(decreasing, 5) >= 5)

  # update output vector
  if(!is.na(test5)){
    if (sum(test5) == 0){
      output$test5 = TRUE
    } else if (sum(test5) >= 1){
      output$test5 = FALSE
    }
  }

  # test 6 #################

  # evaluate whether each point is within 1s
  within_1s = c()
  for (i in 1:length(averages)){
    point = averages[i]
    point_s = sigma[i]
    # if the point is above the centerline
    if (point > xbbar){
      # if the point is within 1s
      if (point < (xbbar + point_s)){ 
        within_1s = append(within_1s, TRUE)
      } else{
        within_1s = append(within_1s, FALSE)
      }
    } else{ # below the centerline
      # if the point is within 1s
      if (point > (xbbar - point_s)){ 
        within_1s = append(within_1s, TRUE)
      } else{
        within_1s = append(within_1s, FALSE)
      }
    }
  }
    
  # count the number of times there are at least 15 values within a window 
  # of length 15 that are within 1s of the centerline
  test6 = sum(window(within_1s, 15) >= 15)

  # update output vector
  if(!is.na(test6)){
    if (sum(test6) == 0){
      output$test6 = TRUE
    } else if (sum(test6) >= 1){
      output$test6 = FALSE
    }
  }

  # test 7 #################

  # evaluate whether each point is outside 1s
  outside_1s = c()
  for (i in 1:length(averages)){
    point = averages[i]
    point_s = sigma[i]
    # if the point is above the centerline
    if (point > xbbar){
      # if the point is outside 1s
      if (point > (xbbar + point_s)){ 
        outside_1s = append(outside_1s, TRUE)
      } else{
        outside_1s = append(outside_1s, FALSE)
      }
    } else{ # below the centerline
      # if the point is outside 1s
      if (point < (xbbar - point_s)){ 
        outside_1s = append(outside_1s, TRUE)
      } else{
        outside_1s = append(outside_1s, FALSE)
      }
    }
  }

  # count the number of times there are at least 8 values within a window 
  # of length 8 that are within 1s of the centerline
  test7 = sum(window(outside_1s, 8) >= 8)

  # update output vector
  if(!is.na(test7)){
    if (sum(test7) == 0){
      output$test7 = TRUE
    } else if (sum(test7) >= 1){
      output$test7 = FALSE
    }
  }

  # test 8 #################

  # evaluate whether the previous point is on the opposide side of the 
  # centerline
  # 0 means didn't cross the centerline 
  # 2 means going from above centerline to below centerline
  # -2 means going from below centerline to above centerline
  changedsign = (sign(averages[-length(averages)] - xbbar)) - 
    (sign(averages[-1] - xbbar))

  # count the number of times there are at least 14 values within a window 
  # of length 14 that are alternating above and below the centerline
  test8 = sum(window(changedsign != 0, 8) >= 8)

  # update output vector
  if(!is.na(test8)){
    if (sum(test8) == 0){
      output$test8 = TRUE
    } else if (sum(test8) >= 1){
      output$test8 = FALSE
    }
  }

  return(output)
}
