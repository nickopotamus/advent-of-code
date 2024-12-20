library(tidyverse)
library(cookiemonster)
library(httr2)

# Data ####

## Example data
example <- c(
  "7 6 4 2 1",
  "1 2 7 8 9",
  "9 7 6 2 1",
  "1 3 2 4 5",
  "8 6 4 4 1",
  "1 3 6 7 9"
)

## Get input data
input_raw <- request("https://adventofcode.com/2024/day/2/input") %>% 
  req_options(cookie = get_cookies("adventofcode.com", as = "string")) %>% 
  req_perform() %>% 
  resp_body_string() 

input <- str_split(input_raw, "\n", simplify = FALSE)[[1]] # Split input into reports (lines)
input <- input[input != ""]                                # Filter empty lines (the last one)

# Part 1 ####

## Check if a single report is safe
is_safe <- function(report_line) {
  
  # Convert space-separated string into a numeric vector
  levels <- str_split(report_line, " ", simplify = TRUE) %>% as.numeric()
  
  # Check if all differences between adjacent levels are within [-3, -1] or [1, 3]
  diffs <- diff(levels)
  valid_diffs <- all(abs(diffs) >= 1 & abs(diffs) <= 3)
  
  # Check if the levels are either strictly increasing or strictly decreasing
  increasing <- all(diffs > 0)
  decreasing <- all(diffs < 0)
  
  return(valid_diffs && (increasing || decreasing))
}

## Check a full dataset of reports
analyse_reports <- function(data) {

  # Apply is_safe() to each report and total number TRUE
  sum(sapply(data, is_safe))
}

## Test
test_safe_reports <- analyse_reports(example)
print(test_safe_reports)

## Result
num_safe_reports <- analyse_reports(input)
print(num_safe_reports)

# Part 2 ####

## Modify above function to include the problem dampener
is_safe_with_dampener <- function(report_line) {
  
  # Convert the space-separated string into a numeric vector
  levels <- str_split(report_line, " ", simplify = TRUE) %>% as.numeric()
  
  # First check if the report is already safe
  if (is_safe(levels)) {
    return(TRUE)
  }
  
  # Check if removing a single level makes the report safe
  for (i in seq_along(levels)) {
    # Remove the ith level and test safety
    modified_levels <- levels[-i]
    if (is_safe(modified_levels)) {
      return(TRUE)
    }
  }
  
  # If no single removal makes it safe, return FALSE
  return(FALSE)
}

analyse_reports_with_dampener <- function(lines) {
  # Apply the `is_safe_with_dampener` function to each report
  sum(sapply(lines, is_safe_with_dampener))
}

## Test
test_safe_reports_with_dampener <- analyse_reports_with_dampener(example)
print(test_safe_reports_with_dampener)

## Result
num_safe_reports_with_dampener <- analyse_reports_with_dampener(input)
print(num_safe_reports_with_dampener)
