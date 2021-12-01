# Day 1: Advent of Code 2020

library(here)
library(tidyverse)

# Part 1
# On how many occasions is the value in the n+1th slot larger than the value in the nth slot?

#input_filename <- "day01_test.txt"
input_filename <- "day01_input.txt"

my_data <- as.numeric(readLines(here("data", input_filename)))
part1 <- sum(diff(my_data) > 0)


# Part 2
#"Now, consider sums of a three-measurement sliding window e.g.
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
#Count the number of times the sum of measurements in this sliding window increases"

# We actually want to know how many times the n+3th value is larger than the nth value
# as the other two values are shared between the sums - don't need to calculate sums or write a loop


# Original part 2 solution

# Work out the indices I need to offset the data by 3 and make both vectors the same length
# note: doing calculations inside [] is not reliable so do them here instead
window_len <- 3
start_index <- window_len + 1
curr_len <- length(my_data)
new_len <- curr_len - window_len

# Make vectors to subtract from each other
my_data_offset <- my_data[start_index:curr_len]
my_data_trunc <- my_data[1:new_len]

# Get the answer
part2 <- sum((my_data_offset - my_data_trunc) > 0)


####### Notes after looking at others' solutions #######

#Revised part 2 solution - take 1

# The lead and lag functions from dplyr are designed to offset vectors
# And they pad with NA so we don't have vectors of different lengths - then can use na.omit
# A more efficient part 2 solution would be:

window_len <- 3
my_data_offset2 <- dplyr::lead(my_data, window_len)
part2a <- sum(na.omit(my_data_offset2 - my_data) > 0)


#Revised part 2 solution - take 2
# The diff function allows you to specify an offset, meaning that the syntax can be more or less identical to the part 1 solution

window_len <- 3
part2b <- sum(diff(my_data, lag = window_len) > 0)


# Revised part 2 solution - take 3
# Turns out you can do arithmetic in square brackets by putting it inside round brackets
# So my original solution could have been expressed like so:

window_len <- 3
my_data_offset <- my_data[(window_len + 1):(length(my_data))]
my_data_trunc <- my_data[1:(length(my_data) - window_len)]

part2c <- sum((my_data_offset - my_data_trunc) > 0)


