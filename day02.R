# Day 1: Advent of Code 2020

library(here)
library(tidyverse)

# Part 1

# Here we can move in two independent directions:
#   forward (at the moment there is no backward)
#   up or down - down is +ve as we are in a submarine

# Input has format
# forward 5
# down 5
# forward 8
# up 3
# down 8
# forward 2

# Part 1 answer is horizontal position * depth, assuming that we start at 0,0

# Parse input
#input_filename <- "day02_test.txt"
input_filename <- "day02_input.txt"

# Read in data and create new columns with axis of travel (x or z) and  vector version of value
my_data <- read_delim(here("data", input_filename), col_names = c("dirn", "mag"), delim = " ") %>%
  mutate(axis = ifelse(dirn == "forward", "x", "z")) %>%
  mutate(vec = ifelse(dirn == "forward" | dirn == "down", mag, -mag))

# Sum movement along each axis
part1_sums <- my_data %>%
  group_by(axis) %>%
  summarise(my_sum = sum(vec))

# Final answer is x position * z position
part1 <- prod(part1_sums$my_sum)
part1

# Part 2
# Turns out that the instructions actually mean something else (surprise!)
# "down X increases your aim by X units.
# up X decreases your aim by X units.
# forward X does two things:
#   It increases your horizontal position by X units.
#   It increases your depth by your aim multiplied by X."

# aim is independent of position and is a cumulative sum
# dplyr lets you do cumulative sums on groups

part2_df <- my_data %>%
  # Calculate aim if dirn is up or down
  group_by(axis) %>%
  mutate(aim = ifelse(axis == "z", cumsum(vec), NA)) %>%
  ungroup() %>%
  # Fill aim down and replace initial NAs with 0
  fill(aim, .direction = "down") %>%
  replace_na(list (aim =  0)) %>%
  # Calculate change in position row by row
  mutate(delta_x = ifelse(axis == "x", mag, 0)) %>%
  mutate(delta_z = ifelse(axis == "x", mag*aim, 0))

# Final answer is x position * z position
part2 <- sum(part2_df$delta_x) * sum(part2_df$delta_z)
part2