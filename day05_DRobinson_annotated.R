# Day 05 - annotated version of David Robinson's solution
# Put this together as he used all sorts of functions I had not encountered before
# https://twitter.com/drob/status/1467361848525787138/photo/1

# Note that nest overrides group

library(here)
library(tidyverse)

input_filename <- "day05_test.txt"
#input_filename <- "day05_input.txt"

data_orig <- as_tibble(read_lines(here("data", input_filename))) %>%
  # extract is new to me - it pattern matches the input and looks like it does an amazing job
  extract(value, c("x1", "y1", "x2", "y2"), "(\\d+),(\\d+) -> (\\d+),(\\d+)")

# This bit gives us the sequences from x1:x2 and y1:y2 in a new column
# map2 is very powerful!
data <- data_orig %>%
  mutate(x = map2(x1, x2, seq)) %>%
  mutate(y = map2(y1, y2, seq))

# Part 1 - horizonntal and vertical lines
# The first unnest makes one row per value that was in a sequence in x
# The second unnest makes one row per value that was a sequence in y
horizontal_vertical <- data %>%
  # Select only horizontal and vertical lines
  filter (x1 == x2 | y1 == y2) %>%
  unnest(x) %>%
  unnest(y)

# Now we have one row per point that a line passes through, and we count them (like I did)
horizontal_vertical <- horizontal_vertical %>%
  count(x, y) %>%
  summarize(sum(n >= 2))

part1 <- pull(horizontal_vertical)

# For part 2 we select only the diagonal lines then unnest them together
# This goes along both sequences one at a time simultaneously, which is exactly what we need
# 1:3, 3:1 unnested together will yield 1,3 2,3 3,1
diagonal <- data %>%
  filter (!(x1 == x2 | y1 == y2)) %>%
  unnest(c(x, y))





