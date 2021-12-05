# Day 5: Advent of Code 2020

library(here)
library(tidyverse)

#input_filename <- "day05_test.txt"
input_filename <- "day05_input.txt"

# We have pairs of x,y co-ordinates that are joined together to make lines, one per line in the textfile: 427,523 -> 427,790
# Consider only the horizontal and vertical lines
# At how many locations do at least 2 lines coincide

# Function returns all points along a horizontal, vertical or diagonal line
line_points <- function(x1, y1, x2, y2){
  if(x1 == x2){
    y_vals <- y1:y2
    x_vals <- rep(x1, length(y_vals))
    return(cbind(x_vals, y_vals))
  }
  if (y1 == y2){
    x_vals <- x1:x2
    y_vals <- rep(y1, length(x_vals))
    return(cbind(x_vals, y_vals))
  } else { # diagonal line
    x_vals <- NA
    y_vals <- NA
    # Check which direction we are going in
    if ((x1<x2 & y1<y2) | (x1>x2 & y1>y2)){
      # slopes up right
      for(i in seq_along(min(x1, x2):max(x1, x2))){
          x_vals[i] <- min(x1, x2) + i - 1
          y_vals[i] <- min(y1, y2) + i - 1
        }
    } else {
      # slopes down right
      for(i in seq_along(min(x1, x2):max(x1, x2))){
        x_vals[i] <- min(x1, x2) + i - 1
        y_vals[i] <- max(y1, y2) - i  + 1
      }      
    }
    return(cbind(x_vals, y_vals))
  }
}


# Parse input
my_data <- read_csv(here("data", input_filename), col_names = c("x1", "muddle", "y2")) %>%
  separate(muddle, into = c("y1", "x2"), sep =  " -> ") %>%
  mutate(across(where(is.character), as.numeric))

# R indexes from 1
my_data <- my_data + (1-min(my_data))  

# Part 1
# Keep rows that contain horizontal or vertical lines
p1_data <- my_data %>%
  filter(x1 == x2 | y1 == y2)

# Use R's index notation to fill in a grid with lines
my_grid <- matrix(data = 0,
                  nrow = max(c(p1_data$y1, p1_data$y2)),
                  ncol = max(c(p1_data$x1, p1_data$x2)))

# Draw lines (in a way that does not make multiple copies of the grid)
for(i in seq_along(p1_data$x1)){
  my_grid[p1_data$y1[i]:p1_data$y2[i], p1_data$x1[i]:p1_data$x2[i]] =
    my_grid[p1_data$y1[i]:p1_data$y2[i], p1_data$x1[i]:p1_data$x2[i]] + 1
}

# How many locations have a value of 2 or more?
part1 <- sum(my_grid >= 2)
part1


# Part 2: turns out that the input contains only horizontal, vertical and diagonal lines
# An entry like 1,1 -> 3,3 covers points 1,1, 2,2, and 3,3
# We need to do the same again - find all points where 2 lines overlap

# Generate co-ordinates of all points on lines then count how many times each co-ordinate appears
for(i in seq_along(my_data$x1)){
  this_line <- line_points(my_data$x1[i], my_data$y1[i], my_data$x2[i], my_data$y2[i])
  if (i == 1){
    all_lines <- this_line
  } else {
    all_lines <- rbind(all_lines, this_line)
  }
}

all_lines_count <- as_tibble(all_lines) %>%
  group_by(x_vals, y_vals) %>%
  tally() %>%
  filter(n >= 2)

part2 <- length(all_lines_count$n)
part2
