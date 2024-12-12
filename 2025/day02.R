library(tidyverse)
library(cookiemonster)
library(httr2)

# Part 1 ####

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

# Split input into reports (lines)
input <- str_split(input_raw, "\n", simplify = FALSE)[[1]]

# Filter empty lines (the last one)
input <- input[input != ""]

## Check if a single report is safe
is_safe <- function(report_line) {
  
  # Convert the space-separated string into a numeric vector
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

  # Apply the `is_safe` function to each report
  sum(sapply(data, is_safe))
}

## Test
test_safe_reports <- analyse_reports(example)
print(test_safe_reports)

## Result
num_safe_reports <- analyse_reports(input)
print(num_safe_reports)

# Part 2 ####