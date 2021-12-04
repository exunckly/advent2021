# Day 4: Advent of Code 2020

library(here)
library(tidyverse)

# Part 1

#

# Parse input
#input_filename <- "day04_test.txt"
input_filename <- "day04_input.txt"

# Represent each bingo card as an array from 1:25 (doesn't need to be 2D) 
# Pregenerate a list of locations that we want to match to win
# then use %in% a couple of times

# Matrix of bingo cards
# Matrix of matched locations
# Matrix of winning locations (e.g. 1,2,3,4,5   1,6,11,16,21)

# Bingo card dimensions
# Will assume here that bingo cards are always square - seems reasonable here given the input data
grid_x <- 5
grid_y <-5

# Function to generate locations of all winning lines of an x by y bingo card (e.g. 1,2,3,4,5   1,6,11,16,21)
# Written for square cards but may need to revisit code on a later day
gen_win_lines <- function(x, y){
  # We win if our locations are all %in% one of the rows in this grid
  hor_lines <- matrix(data = seq(1, x*y, by = 1), nrow = y, ncol = x)
  ver_lines <- t(hor_lines)
  win_lines <- rbind(ver_lines, hor_lines) # this line fails for rectangular bingo cards
  return(win_lines)
}

# Function to test whether a vector of locations contains a win
win_test <- function(test_locs, win_lines){
  # Infer the size of the grid from the dimensions of win_lines
  grid_size <- dim(win_lines)[2]
  my_test <- matrix(win_lines %in% test_locs, dim(win_lines)[1], dim(win_lines)[2])
  # Is there a row that is all TRUE
  return(grid_size %in% rowSums(my_test))
}


# Parse input (this is pretty horrible at the moment)
# We have a sequence of bingo numbers in line 1, followed by many 5x5 bingo cards

# Get the bingo numbers into a vector
bingo_numbers <- read_lines(here("data", input_filename), n_max = 1) %>%
  str_split(",") %>%
  unlist() %>%
  as.numeric()

# Cards as a matrix with one card per row
bingo_cards <- as_tibble(read_lines(here("data", input_filename), skip = 2)) %>%
  mutate(value = str_trim(value, side = "left")) %>% # Remove leftmost whitespace
  mutate(value = str_replace_all(value, "[\\s]+", ",")) # Replace other whitespace with comma

messy <- format_csv(bingo_cards) %>%
  str_replace_all("\n\n", "my_placeholder") %>%
  str_replace_all("\"\n\"", ",") %>% # concatenate different lines on same card together
  str_replace_all("my_placeholder", "\n") %>%
  str_replace_all(",", "\",\"") %>% # one column per value
  str_replace(".*\n", "") # remove old column name

bingo_cards_reformatted <- messy %>%
  read_csv(col_names = FALSE) %>%
  as.matrix()

# Generate winning lines 
my_winlines <- gen_win_lines(grid_x, grid_y)

# Set up matrix to hold indices of where each called number is on each card
winning_board <- NA


# Function to find winning cards, in order
# Returns array of card ID, number called and sum of unmatched numbers
# Chose those things to return as AoC solution is: number just called * sum of all unmarked numbers on board

find_win_order <- function(my_bingo_numbers, my_bingo_cards){
  # Set up array to hold indices of matching numbers for each card
  blank_matches <- rep(NA, length(my_bingo_numbers))
  bingo_matches <- matrix(rep(blank_matches, dim(my_bingo_cards)[1]),
                          nrow = dim(my_bingo_cards)[1],
                          ncol = length(bingo_numbers))
  
  # Set up structures to hold info on winning cards
  no_wins <- 0
  winning_card_ids <- rep(NA, dim(my_bingo_cards)[1])
  winning_numbers_called <- rep(NA, dim(my_bingo_cards)[1])
  unmarked_sum_at_time_of_win <- rep(NA, dim(my_bingo_cards)[1])
  
  # Call numbers one at a time to find winning card
  for(i in seq_along(my_bingo_numbers)){
    for(j in 1:(dim(my_bingo_cards)[1])){
      # Test to see if the number just called (i) is on card j
      # If it is then get the index of the location where i is
      if(any(my_bingo_cards[j,] %in% my_bingo_numbers[i])){
        index <- which(my_bingo_cards[j,] %in% my_bingo_numbers[i])
        first_na <- (min(which(is.na(bingo_matches[j,]))))
        bingo_matches[j,first_na] <- index 
      }
      # Test to see if we have a winning card versus the matrix of winning locations we set up previously
      if(win_test(na.omit(bingo_matches[j,]), my_winlines)){
        if(!j %in% winning_card_ids){
          no_wins <- no_wins + 1
          winning_card_ids[no_wins] <- j
          winning_numbers_called[no_wins] <- my_bingo_numbers[i]
          unmarked_sum_at_time_of_win[no_wins] <- sum(bingo_cards_reformatted[j,]) - 
            sum(na.omit(bingo_cards_reformatted[j,bingo_matches[j,]]))
        }
      }
    }
  }
  return(rbind(winning_card_ids, winning_numbers_called, unmarked_sum_at_time_of_win))
}


winners <- find_win_order(bingo_numbers, bingo_cards_reformatted)

#Part 1 solution is the first card to win and part 2 solution is the last card to win
part_1 <- winners[2,1] * winners[3,1]

# For part 2, not all cards may win but this works as vectors were initialised with NA
part_2 <- tail(na.omit(winners[2,]), n = 1) * tail(na.omit(winners[3,]), n = 1)
