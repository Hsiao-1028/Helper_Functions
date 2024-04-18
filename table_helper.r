###############################################################################
# -*- encoding: UTF-8 -*-                                                     #
# Author: Po-Kang Hsiao (ted1997623@gmail.com)                                #
# Description: A Simple Row to Table Function                                 #
#                                                                             #
# Last Modified: 2024-04-17                                                   #
###############################################################################

# This function helps you generate a summary statistic table
library(data.table)
library(tidyverse)

rm(list=ls());gc()
# row_table Function: Generates a summary table of frequencies or proportions based on a specified variable.
# @param dt A data.table object containing the data.
# @param var A character string specifying the variable to analyze.
# @param prop Logical indicating if counts should be converted to proportions (default is FALSE).
# @param prop.digit Integer specifying the number of decimal places for proportions (default is 3).
row_table <- function(dt, var, prop = FALSE, prop.digit = 3, out_from_nhi = FALSE) {

  temp_dt <- setDT(dt)
  temp_dt <- temp_dt[, .N, by = get(var)] %>% dcast(formula = ... ~ get, value.var = "N")
  temp_dt[, . := NULL]
  temp_dt[, (names(temp_dt)) := lapply(.SD, function(x) if (is.factor(x)) as.numeric(as.character(x)) else as.numeric(x))]
  temp_dt <- row_table_order(temp_dt)
  
  if (prop) {
    temp_dt[1] <- round(temp_dt[1] / nrow(dt), prop.digit)
  }
  
  if (out_from_nhi){
    temp_dt[temp_dt <=3] <- "<3"
    
  }
  
  return(temp_dt)
}

# row_table_order Function: Reorders columns of a data.table ensuring NA-like columns are placed last and optionally replaces NA values with zero.
# @param dt A data.table object to reorder.
# @param NA.to.zero Logical indicating whether NA values should be replaced with zero (default is FALSE).
row_table_order <- function(dt, NA.to.zero = FALSE) {

  na_like_columns <- grep("^[nN][aA]$", names(dt), value = TRUE)
  other_columns <- setdiff(names(dt), na_like_columns) %>% sort()
  new_order <- c(other_columns, na_like_columns)
  temp_dt <- dt[, ..new_order]
    if (NA.to.zero) {
    temp_dt[, (names(temp_dt)) := lapply(.SD, function(x) ifelse(is.na(x), 0, x))]
  }
  
  return(temp_dt)
}

################################################################################
# Example Usage

# Create example data
group <- sample(c("A", "B", "C", "E", "Z"), 5, replace = TRUE)
group2 <- sample(c("A", "B", "C", "D", NA, "Y"), 323, replace = TRUE)

# Create a data.table
dt <- data.table(group = group)[, dt := 1]
dt2 <- data.table(group = group2)[, dt := 2]
dt_main <- rbind(dt, dt2)


results<- data.table()

for( i in c(1,2) ){
 temp <- dt_main[ dt == i]
 temp <- row_table(temp, "group", prop = FALSE, out_from_nhi = FALSE)
 results <- rbind(results, temp, fill = TRUE) %>% row_table_order( NA.to.zero = F)
}






