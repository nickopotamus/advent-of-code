library(tidyverse)

# Data ####

example_map <- 
"....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."

input_map <- request("https://adventofcode.com/2024/day/6/input") %>% 
  req_options(cookie = get_cookies("adventofcode.com", as = "string")) %>% 
  req_perform() %>% 
  resp_body_string() %>% 
  str_trim() 

# Utility Functions ####

# Parse the input map into a matrix of characters
parse_map <- function(input_map) {
  str_split(input_map, "\n", simplify = TRUE)
}

# Directions: Up, Right, Down, Left
directions <- list("U" = c(-1, 0), "R" = c(0, 1), "D" = c(1, 0), "L" = c(0, -1))

# Turn the guard 90deg right
turn_right <- function(dir) {
  switch(dir,
         "U" = "R",
         "R" = "D",
         "D" = "L",
         "L" = "U")
}

# Function to check if the position is within bounds
in_bounds <- function(pos) {
  r <- pos[1]
  c <- pos[2]
  r > 0 && r <= rows && c > 0 && c <= cols
}

# Function to check if there is an obstacle
is_obstacle <- function(pos, temp_map) {
  r <- pos[1]
  c <- pos[2]
  substr(temp_map[r], c, c) == "#"
}

# Count the total number of "." in the map
# So I can implement a counter of potential positions!
count_dots <- function(map) {
  total_dots <- 0
  for (r in 1:nrow(map)) {
    row <- map[r]
    # Loop through each character in the row
    for (c in seq_len(nchar(row))) {
      if (substr(row, c, c) == ".") {
        total_dots <- total_dots + 1
      }
    }
  }
  return(total_dots)
}

# Part 1 ####
simulate_guard_patrol <- function(input_map) {
  
  # Parse the input map
  map <- parse_map(input_map)
  rows <- length(map)
  cols <- nchar(map[1])
  
  # Find the guard's starting position and initial direction
  guard_pos <- NULL
  guard_dir <- NULL
  for (r in 1:rows) {
    for (c in 1:cols) {
      if (substr(map[r], c, c) %in% c("^", ">", "v", "<")) {
        guard_pos <- c(r, c)
        guard_dir <- switch(substr(map[r], c, c),
                            "^" = "U",
                            ">" = "R",
                            "v" = "D",
                            "<" = "L")
        break
      }
    }
    if (!is.null(guard_pos)) break
  }
  
  # Initialize visited positions as a list
  visited <- list(paste(guard_pos, collapse = ","))
  
  # Simulate the patrol
  repeat {
    
    # Calculate the position in front of the guard
    move <- directions[[guard_dir]]
    front_pos <- guard_pos + move
    
    if (!in_bounds(front_pos)) break  # Guard leaves the map
    
    if (is_obstacle(front_pos)) {
      # Turn right if there's an obstacle
      guard_dir <- turn_right(guard_dir)
    } else {
      # Move forward if there's no obstacle
      guard_pos <- front_pos
      # Add the new position to visited
      visited <- c(visited, paste(guard_pos, collapse = ","))
    }
  }
  
  # Return the number of distinct positions visited
  length(unique(visited))
}

## Results ####
example_result <- simulate_guard_patrol(example_map)
print(example_result)

input_result <- simulate_guard_patrol(input_map)
print(input_result)

# Part 2 ####

simulate_guard_with_obstruction <- function(input_map) {
  
  # Parse the input map
  map <- str_split(input_map, "\n", simplify = TRUE)
  rows <- length(map)
  cols <- nchar(map[1])
  
  # Find the guard's starting position and initial direction
  guard_pos <- NULL
  guard_dir <- NULL
  for (r in 1:rows) {
    for (c in 1:cols) {
      if (substr(map[r], c, c) %in% c("^", ">", "v", "<")) {
        guard_pos <- c(r, c)
        guard_dir <- switch(substr(map[r], c, c),
                            "^" = "U",
                            ">" = "R",
                            "v" = "D",
                            "<" = "L")
        break
      }
    }
    if (!is.null(guard_pos)) break
  }
  
  # Function to simulate the patrol
  simulate_patrol <- function(temp_map) {
    guard_pos <- guard_pos
    guard_dir <- guard_dir
    visited <- list()
    
    repeat {
      move <- directions[[guard_dir]]
      front_pos <- guard_pos + move
      
      if (!in_bounds(front_pos)) break  # Guard leaves the map
      
      if (is_obstacle(front_pos, temp_map)) {
        # Turn right if there's an obstacle
        guard_dir <- turn_right(guard_dir)
      } else {
        # Move forward if there's no obstacle
        guard_pos <- front_pos
        visit_key <- paste(guard_pos, guard_dir, collapse = ",")
        if (visit_key %in% visited) return(TRUE)  # Loop detected
        visited <- c(visited, visit_key)
      }
    }
    return(FALSE)
  }
  
  # Test adding an obstruction at every empty position
  total_positions <- rows*cols - count_dots(map)
  explored <- 0
  loop_positions <- 0
  
  for (r in 1:rows) {
    for (c in 1:cols) {
      if (substr(map[r], c, c) == "." && !(r == guard_pos[1] && c == guard_pos[2])) {
        # Create a temporary map with an obstruction at (r, c)
        temp_map <- map
        substr(temp_map[r], c, c) <- "#"
        
        # Simulate the patrol
        if (simulate_patrol(temp_map)) {
          loop_positions <- loop_positions + 1
        }
        
        # Update progress counter
        explored <- explored + 1
        cat(sprintf("\rExplored: %.2f%%", (explored / total_positions) * 100))
        flush.console()
      }
    }
  }
  
  cat("\n")  # Move to the next line after the progress is complete
  
  return(loop_positions)
}

## Results ####
example_result <- simulate_guard_with_obstruction(example_map)
print(example_result)

input_result <- simulate_guard_with_obstruction(input_map)
print(input_result)

