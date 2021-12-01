# Day 1: Advent of Code 2020

library(here)
library(tidyverse)

# Part 1
# On how many occasions is the value in the n+1th slot larger than the value in the nth slot?

#input_filename <- "day01_test.txt"
input_filename <- "day01_input.txt"

my_data <- as.vector(as.numeric(readLines(here("data", input_filename))))
my_signs <- sign(diff(my_data))

part1 <- sum(my_signs == 1)


# Part 2
#Now, consider sums of a three-measurement sliding window e.g.
  
# 199  A      
# 200  A B    
# 208  A B C  
# 210    B C D
# 200  E   C D
# 207  E F   D
# 240  E F G  
# 269    F G H
# 260      G H
# 263        H
#Count the number of times the sum of measurements in this sliding window increases 


# We actually want to know how many times the n+3th value is larger than the nth value
# as the other two values are shared between the sums - don't need to calculate sums or write a loop

# Work out the indices I need to offset the data by 3 and make both vectors the same length
window_len <- 3
start_index <- window_len + 1
curr_len <- length(my_data)
new_len <- curr_len - window_len

# Make vectors to subtract from each other
my_data_offset <- my_data[start_index:curr_len]
my_data_trunc <- my_data[1:new_len]
my_signs_part2 <- sign(my_data_offset - my_data_trunc)

part2 <- sum(my_signs_part2 == 1)


