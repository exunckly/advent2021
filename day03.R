# Day 3: Advent of Code 2020

library(here)
library(tidyverse)

# Part 1

# We are given some binary numbers and asked to find the most common digit in each piosition, and the inverse (least common digit)
# Then put those digits together to make gamma (most common) and epsilon (least common)
# Numbers al contain nthe same number of digits, but number of digits varies betweebn test and actual input
# Final answer is gamma * epsilon, in decimal


# Parse input
#input_filename <- "day02_test.txt"
input_filename <- "day02_input.txt"

# The most common value is the mode, this feels quicker than summing each column and seeing if the answwer is higher than 

my_data <- read_csv(here("data", input_filename), col_names = FALSE)
