# Day 6: Advent of Code 2020

library(here)
library(tidyverse)

#input_filename <- "day06_test.txt"
input_filename <- "day06_input.txt"

# Problem from the AoC website
# Suppose you have a lanternfish with an internal timer value of 3:
#    After one day, its internal timer would become 2.
#    After another day, its internal timer would become 1.
#    After another day, its internal timer would become 0.
#    After another day, its internal timer would reset to 6, and it would create a new lanternfish with an internal timer of 8.
#    After another day, the first lanternfish would have an internal timer of 5, and the second lanternfish would have an internal timer of 7.
#    A lanternfish that creates a new fish resets its timer to 6, not 7 (because 0 is included as a valid timer value
#          The new lanternfish starts with an internal timer of 8 and does not start counting down until the next day.)

# Each day, a 0 becomes a 6 and adds a new 8 to the end of the list,
# while each other number decreases by 1 if it was present at the start of the day.
# Input to problem will look like 3,4,3,1,2 (on dqy 0)

# A vector that we append numbers to doesn't look like a very efficient way of representing the problem as it won't scale very well.
# e.g. after 80 days the example above has 5934 fish on day 80

fish_sim <-  function(initial_state, days, reset_start_value, new_start_value, no_offspring = 1){
  # Keep track of the /number of instances/ of fish with each state rather than the individual fish themselves 
    # days is the number of days we run the sim for (initial state is day 0)
    # reset_value is the value a fish resets to after having offspring
    # new_start_value is the value that new fish start with
    # no_offspring is the number of offspring each fish has when it reaches zero

  # Keep track of number of fish with each index at a given timein casse we need more than the total for part 2
  fish_count <- matrix(rep(0, (days + 1) * (new_start_value + 1)), nrow = days+1)
  
  # Convert initial state to count of number of fish in each state as a vector (allowing for missing numbers)
  fish_counts_initial <- initial_state %>%
    as.tibble() %>%
    count(value)
  initial_state_lookup <- as.tibble(c(0:(new_start_value))) %>%
    left_join(fish_counts_initial, by = "value") %>%
    replace(is.na(.), 0)
  
  # Transfer initial state to first row of matrix
  fish_count[1,1:(new_start_value + 1)] <- initial_state_lookup$n # Set it up to track a single fish

  # Work out what happens one day at a time
  for (i in 2:(days+1)){
    # Each fish that we already have moves one place (plaice!!!) to the left
    fish_count_temp <- lead(fish_count[(i-1),],1, default = 0)
    # Under the state that new fish start in we have no_offspring * number of fish in the leftmost position on the previous iteration
    fish_count_temp[new_start_value + 1] <- fish_count[(i-1),1] * no_offspring
    # We reset the fish that were previously zero
    fish_count_temp[reset_start_value + 1] = fish_count_temp[reset_start_value + 1] + fish_count[(i-1),1]
    fish_count[i,] <- fish_count_temp
  }
  return(fish_count)
}

# Part 1
# Work out what happens for one fish that starts at zero

# Get initial state of fish from file (one value per fish)
my_data <- read_lines(here("data", input_filename)) %>%
  str_split(pattern = ",") %>%
  unlist() %>%
  as.numeric()

no_days <- 80
part1_fish <- fish_sim(my_data, no_days, reset_start_value = 6, new_start_value = 8, no_offspring = 1)
part1 <- sum(part1_fish[(no_days+1),])
part1

# Part 2 - 256 days

no_days <- 256 # hahaha nope
part2_fish <- fish_sim(my_data, no_days, reset_start_value = 6, new_start_value = 8, no_offspring = 1)
part2 <- sum(part2_fish[(no_days+1),])
sprintf("%.0f",part2)



