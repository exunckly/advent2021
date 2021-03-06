# Day 3: Advent of Code 2020

library(here)
library(tidyverse)

# Part 1

# We are given some binary numbers and asked to find the most common digit in each position, and the inverse (least common digit)
# Then put those digits together to make gamma (most common) and epsilon (least common)
# Numbers all contain the same number of digits, but number of digits varies between test and actual input
# Final answer is gamma * epsilon, in decimal


# Parse input
#input_filename <- "day03_test.txt"
input_filename <- "day03_input.txt"

# The most common value is the mode
# R does not have a built in mode function
# This function finds the mode of a vector. If there is a tie it returns the largest value by default,
# but we can also return the smallest value if we wish 
my_mode <- function(v, keep = "hi") {
  uniqv <- unique(v)
  tabulatedv <- tabulate(match(v, uniqv))
  my_modes <- uniqv[which(tabulatedv == max(tabulatedv))]
  if(keep == "hi"){
    return(max(my_modes))
  } else if(keep == "low"){
    return(min(my_modes))
  }
}

# Function converts a vector of 1s and zeros to an integer
bin_vec_to_int <- function(x){
  # We don't test if input contains only 1s and 0s as life is too short
  paste(x, collapse = "") %>%
    strtoi(base = 2)
}

# Prepare data
my_data <- read_csv(here("data", input_filename), col_names = FALSE) %>%
  mutate(across(where(is.character), str_trim)) 
  
my_colnames <- paste0("B", c(0:(nchar(my_data$X1[1]))))

my_data <- my_data %>%
  separate(X1, sep = "", into = my_colnames) %>%
  mutate(across(where(is.character), as.numeric)) %>%
  select(-B0)

# Gamma is most common value in each column
gamma_vec <- my_data %>%
  summarise(across(where(is.numeric), my_mode)) %>%
  unlist(., use.names=FALSE)

# Epsilon is inverse of gamma
epsilon_vec <- +(!gamma_vec)

# Do the maths
gamma <- bin_vec_to_int(gamma_vec)
epsilon <- bin_vec_to_int(epsilon_vec)
part1 <- gamma * epsilon
part1


# Part 2
# Now we have to find the most common (O2) and least common (CO2) value in each position and filter to keep only rows containing that value
# in that position
# We stop when there is only one row left

# if there is a tie keep 1 for O2 and 0 for CO2

o2_data <- my_data
co2_data <- my_data

i <- 1
while(i <= length(my_data)){
  if(length(o2_data$B1) == 1){break} 
  check_val <- my_mode(unlist(o2_data[,i]), keep = "hi")
  o2_data <- o2_data %>%
    filter(.[,i] == check_val)
  i <- i + 1
}


i <- 1
while(i <= length(my_data)){
  if(length(co2_data$B1) == 1){break} 
  check_val <- +(!my_mode(unlist(co2_data[,i]), keep = "hi"))
  co2_data <- co2_data %>%
    filter(.[,i] == check_val)
  i <- i + 1
}

# Do the maths
o2_val<- bin_vec_to_int(o2_data)
co2_val<- bin_vec_to_int(co2_data)
part2 <- o2_val * co2_val
part2
