library(tidyverse)
library(cookiemonster)
library(httr2)

# Data ####

## Example

example_text <- 
"47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"

input_text <- request("https://adventofcode.com/2024/day/5/input") %>% 
  req_options(cookie = get_cookies("adventofcode.com", as = "string")) %>% 
  req_perform() %>% 
  resp_body_string() %>% 
  str_trim() 

# Part 1 ####

## 1) Parse inputs ####

parse_input <- function(input_text) {
  # Split the input into rules and updates
  sections <- str_split(input_text, "\n\n", simplify = TRUE)
  
  # Parse ordering rules into a tibble
  ordering_rules <- str_split(sections[1], "\n")[[1]] %>%
    map(~ str_split(.x, "\\|", simplify = TRUE) %>% as.integer()) %>%
    map_df(~ tibble(x = .x[1], y = .x[2]))
  
  # Parse updates into a list of vectors
  updates <- str_split(sections[2], "\n")[[1]] %>%
    map(~ str_split(.x, ",", simplify = TRUE) %>% as.integer())
  
  list(ordering_rules = ordering_rules, updates = updates)
}

## 2) Validate updates ####

is_update_valid <- function(update, ordering_rules) {
  
  # Return FALSE for invalid updates
  if (is.null(update) || length(update) == 0 || any(is.na(update))) {
    message("Invalid update: NULL or contains NA.")
    return(FALSE)
  }
  
  # Filter the rules to include only those relevant for the current update
  update_set <- unique(update)
  relevant_rules <- ordering_rules %>%
    filter(x %in% update_set, y %in% update_set)
  
  # Debug: Show relevant rules
  message("Relevant rules: ", paste(update, collapse = ", "))
  #print(relevant_rules)
  
  # Map the update to a named index for quick lookup
  page_indices <- set_names(seq_along(update), update)
  
  # Debug: Show page indices
  #message("Page indices for update: ")
  #print(page_indices)
  
  # Validate rules
  validation_results <- relevant_rules %>%
    rowwise() %>%
    mutate(
      x_index = page_indices[as.character(x)],
      y_index = page_indices[as.character(y)],
      valid = x_index < y_index
    ) %>%
    ungroup()
  
  # Debug: Show validation results
  #message("Validation results for update: ", paste(update, collapse = ", "))
  print(validation_results)
  
  # Summarise validation results
  all_valid <- validation_results %>%
    summarise(all_valid = all(valid, na.rm = TRUE)) %>%
    pull(all_valid)
  
  # Return TRUE if all rules are satisfied, FALSE otherwise
  return(all_valid)
}


## 3) Find middle page ####

# Got this wrong the first time - unsorted order!
# find_middle_page <- function(update) {
#   
# Sort the update to find the middle page
#   sorted_update <- sort(update)
#   
#   # Debug: Show sorted update
#   message("Sorted update: ", paste(sorted_update, collapse = ", "))
#   
#   # Compute the middle page
#   middle <- sorted_update[ceiling(length(sorted_update) / 2)]
#   
#   # Debug: Show middle page
#   message("Middle page for update: ", middle)
#   
#   return(middle)
# }

find_middle_page <- function(update) {
  
  # Compute the middle page based on the original order
  middle <- update[ceiling(length(update) / 2)]
  
  # Debug: Show middle page
  message("Middle page for update: ", middle)
  
  return(middle)
}


## 4) Put it all together and compute the result ####

process_updates <- function(input_text) {
  
  # Parse the input
  parsed <- parse_input(input_text)
  ordering_rules <- parsed$ordering_rules
  updates <- parsed$updates
  
  # Validate updates and compute the middle pages for valid updates
  middle_pages <- updates %>%
    map(~ {
      if (is.null(.x) || length(.x) == 0 || any(is.na(.x))) {
        message("Invalid update encountered: ", paste(.x, collapse = ", "))
        return(NA) # Return NA for invalid updates
      }
      valid <- is_update_valid(.x, ordering_rules)
      if (!valid) {
        message("Update failed validation: ", paste(.x, collapse = ", "))
        return(NA)
      } else {
        message("Valid update: ", paste(.x, collapse = ", "))
        return(find_middle_page(.x))
      }
    }) %>%
    discard(is.na) # Remove NA values (invalid updates)
  
  # Compute the sum of middle pages
  result <- sum(unlist(middle_pages))
  message("Sum of middle pages: ", result)
  
  return(result)
}

## Results ####
process_updates(example_text)
process_updates(input_text)

# Part 2 ####

## Approach ####
# 1) Filter out invalid updates (use above functions)
# 2) Fix invalid updates using topological sort
# 3) Compute middle pages and sum

## Sort algorithm ####

# Topological sort helper function
# 2004's 1A introduction to functional ML was not wasted on me!
topo_sort <- function(nodes, deps) {
  visited <- c()  # Vector to store visited nodes
  result <- c()   # Vector to store the sorted result
  
  # Recursive visit function
  visit <- function(node) {
    if (node %in% visited) return()                       # Skip already visited nodes
    visited <<- c(visited, node)                          # Mark as visited
    if (!is.null(deps[[as.character(node)]])) {
      for (dep in deps[[as.character(node)]]) visit(dep)  # Visit dependencies
    }
    result <<- c(result, node)  # Add node to result
  }
  
  # Visit all nodes
  for (node in nodes) visit(node)
  
  return(rev(result))  # Return the reversed result
}

sort_update <- function(update, ordering_rules) {
  # Apologies for the error handling/debugging - struggled for a while
  # before I realised there was a trailing /n in the input *facepalm*
  
  # Filter relevant rules for this update
  relevant_rules <- ordering_rules %>%
    filter(x %in% update, y %in% update)
  
  # Debug: Show relevant rules
  message("Relevant rules for update: ", paste(update, collapse = ", "))
  print(relevant_rules)
  
  # Create a directed graph as a list of dependencies
  dependencies <- relevant_rules %>%
    group_by(y) %>%
    summarise(before = list(x), .groups = "drop") %>%
    deframe()
  
  # Ensure all nodes are in the dependency graph
  all_nodes <- as.character(update)
  for (node in all_nodes) {
    if (is.null(dependencies[[node]])) {
      dependencies[[node]] <- c()
    }
  }
  
  # Perform topological sort
  sorted <- tryCatch(
    {
      topo_sort(update, dependencies)
    },
    error = function(e) {
      message("Error during topological sort for update: ", paste(update, collapse = ", "))
      message("Error: ", e$message)
      return(NA)
    }
  )
  
  # Return the sorted result or NA if sorting failed
  return(sorted)
}



## Function to process invalid updates and compute the result ####
process_invalid_updates <- function(input_text) {
  
  # Parse the input
  parsed <- parse_input(input_text)
  ordering_rules <- parsed$ordering_rules
  updates <- parsed$updates
  
  # Identify invalid updates
  invalid_updates <- updates %>%
    keep(~ !is_update_valid(.x, ordering_rules))
  
  # Correct the order of invalid updates
  corrected_updates <- invalid_updates %>%
    map(~ sort_update(.x, ordering_rules))
  
  # Compute the middle pages of corrected updates
  middle_pages <- corrected_updates %>%
    map(find_middle_page)
  
  # Compute the sum of middle pages
  result <- sum(unlist(middle_pages))
  message("Sum of middle pages for corrected updates: ", result)
  return(result)
}

## Results ####
process_invalid_updates(example_text)
process_invalid_updates(input_text)