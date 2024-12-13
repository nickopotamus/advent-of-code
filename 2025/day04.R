library(tidyverse)
library(cookiemonster)
library(httr2)

# Data ####

word <- "XMAS"

## Example
test_search <- matrix(
  c(
    "M", "M", "M", "S", "X", "X", "M", "A", "S", "M",
    "M", "S", "A", "M", "X", "M", "S", "M", "S", "A",
    "A", "M", "X", "S", "X", "M", "A", "A", "M", "M",
    "M", "S", "A", "M", "A", "S", "M", "S", "M", "X",
    "X", "M", "A", "S", "A", "M", "X", "A", "M", "M",
    "X", "X", "A", "M", "M", "X", "X", "A", "M", "A",
    "S", "M", "S", "M", "S", "A", "S", "X", "S", "S",
    "S", "A", "X", "A", "M", "A", "S", "A", "A", "A",
    "M", "A", "M", "M", "M", "X", "M", "M", "M", "M",
    "M", "X", "M", "X", "A", "X", "M", "A", "S", "X"
  ),
  byrow = TRUE,
  nrow = 10
)

## Get input data
input <- request("https://adventofcode.com/2024/day/4/input") %>% 
  req_options(cookie = get_cookies("adventofcode.com", as = "string")) %>% 
  req_perform() %>% 
  resp_body_string() %>%
  str_trim() %>%      # Remove trailing whitespace
  str_split("\n") %>% # Split input into lines
  unlist() %>%
  str_split("") %>%   # Split each line into individual characters
  do.call(rbind, .)

# Part 1 ####

# Systematically search grid (as I struggled to code diagonal string extraction)
count_word_occurrences <- function(grid, word) {
  
  word_length <- nchar(word)
  rows <- nrow(grid)
  cols <- ncol(grid)
  total_count <- 0
  
  # Define all possible directions
  directions <- list(
    c(0, 1),    # Right
    c(0, -1),   # Left
    c(1, 0),    # Down
    c(-1, 0),   # Up
    c(1, 1),    # Diagonal Down-Right
    c(-1, -1),  # Diagonal Up-Left
    c(1, -1),   # Diagonal Down-Left
    c(-1, 1)    # Diagonal Up-Right
  )
  
  # Helper function to check if a word exists in a given direction
  is_match <- function(x, y, dx, dy) {
    for (k in seq_len(word_length)) {
      new_x <- x + (k - 1) * dx
      new_y <- y + (k - 1) * dy
      # Check if out of bounds or character mismatch
      if (new_x < 1 || new_x > rows || new_y < 1 || new_y > cols || 
          grid[new_x, new_y] != substr(word, k, k)) {
        return(FALSE)
      }
    }
    return(TRUE)
  }
  
  # Loop over every cell in the grid
  for (i in seq_len(rows)) {
    for (j in seq_len(cols)) {
      # Check all directions from this cell
      for (dir in directions) {
        dx <- dir[1]
        dy <- dir[2]
        if (is_match(i, j, dx, dy)) {
          total_count <- total_count + 1
        }
      }
    }
  }
  
  return(total_count)
}

## Test
test_occurrences <- count_word_occurrences(test_search, word)
print(test_occurrences)

## Result
total_occurrences <- count_word_occurrences(input, word)
print(total_occurrences)


# Part 2 ####

# Function to count occurrences of "X-MAS" in a grid
count_x_mas <- function(grid) {
  
  # Initialise grid
  rows <- nrow(grid)
  cols <- ncol(grid)
  total_count <- 0
  
  # Helper function to check if the "X-MAS" pattern exists
  is_x_mas <- function(i, j) {
    
    # Check if the pattern is within bounds
    if (i < 2 || i > rows - 1 || j < 2 || j > cols - 1) {
      message("Out of bounds at center: (", i, ", ", j, ")")
      return(FALSE)
    }
    
    # Extract positions of interest
    top_left <- grid[i - 1, j - 1]
    top_right <- grid[i - 1, j + 1]
    bottom_left <- grid[i + 1, j - 1]
    bottom_right <- grid[i + 1, j + 1]
    center <- grid[i, j]
    
    # Ensure the center is "A" (optional debugging statement)
    if (center != "A") {
      # message("Invalid center at: (", i, ", ", j, ") -> Center: ", center)
      return(FALSE)
    }
    
    # Form the top and bottom pairs
    top_pair <- paste0(top_left, ".", top_right)
    bottom_pair <- paste0(bottom_left, ".", bottom_right)
    
    # Valid combinations of top and bottom pairs (symmetry rules for the X-MAS)
    valid_combinations <- list(
      "M.S" = "M.S", 
      "S.M" = "S.M", 
      "S.S" = "M.M", 
      "M.M" = "S.S"
    )
    
    # Check if the combination is valid
    if (!is.null(valid_combinations[[top_pair]]) && valid_combinations[[top_pair]] == bottom_pair) {
      return(TRUE)
    }
    
    # Debugging: Log invalid pairs
    message(
      "Invalid at center: (", i, ", ", j, ") -> ",
      "Top: ", top_pair, ", Bottom: ", bottom_pair
    )
    return(FALSE)
  }
  
  # Iterate over the grid to find "X-MAS" patterns
  for (i in 2:(rows - 1)) {
    for (j in 2:(cols - 1)) {
      if (is_x_mas(i, j)) {
        total_count <- total_count + 1
        # Debugging: Log the position of each valid match
        message("X-MAS found at center: (", i, ", ", j, ")")
      }
    }
  }
  
  return(total_count)
}

# Test
test_x_mas <- count_x_mas(test_search)
print(test_x_mas)

total_x_mas <- count_x_mas(input)
print(total_x_mas)

