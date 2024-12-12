library(tidyverse)
library(cookiemonster)
library(httr2)

# Part 1 ####

## Example data
example <- "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

## Get input data
input <- request("https://adventofcode.com/2024/day/3/input") %>% 
  req_options(cookie = get_cookies("adventofcode.com", as = "string")) %>% 
  req_perform() %>% 
  resp_body_string() 

## Extract and process valid mul instructions
process_memory <- function(memory_string) {
  
  # Define a regex pattern to match valid mul instructions
  # Valid format: mul(X,Y) where X and Y are 1-3 digit numbers
  # mul(       : Starts with "mul(".
  # (\\d{1,3}) : First operand, a number with 1 to 3 digits.
  # ,          : Comma separating the two operands.
  # (\\d{1,3}) : Second operand, a number with 1 to 3 digits.
  # ).         : Closing parenthesis.
  pattern <- "mul\\((\\d{1,3}),(\\d{1,3})\\)"
  
  # Extract all matches for the valid pattern
  matches <- str_match_all(memory_string, pattern)[[1]]
  
  # Convert the matched groups to numeric
  operands <- as.data.frame(matches[, -1],   # Drop the text
                            stringsAsFactors = FALSE) %>%
    mutate(across(everything(), as.numeric)) # Ensure numeric
  
  # Calculate the product of the extracted numbers and sum them up
  result <- sum(operands$V1 * operands$V2)
  
  return(result)
}

## Test
test_sum <- process_memory(example)
print(test_sum)

## Result
total_sum <- process_memory(input)
print(total_sum)

# Part 2 ####

example_2 <- "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

## Process corrupted memory with do() and don't() logic
process_memory_with_conditions <- function(memory_string) {
  
  # Define regex patterns
  
  # Matches valid mul instructions
  mul_pattern <- "mul\\((\\d{1,3}),(\\d{1,3})\\)"  
  # Matches do() instructions
  do_pattern <- "do\\(\\)"  
  # Matches don't() instructions
  dont_pattern <- "don't\\(\\)"
  
  # Split memory string into tokens using all possible instructions as delimiters
  tokens <- unlist(strsplit(memory_string, 
                            "(mul\\(\\d{1,3},\\d{1,3}\\)|do\\(\\)|don't\\(\\))", 
                            perl = TRUE))
  
  matches <- gregexpr("(mul\\(\\d{1,3},\\d{1,3}\\)|do\\(\\)|don't\\(\\))", 
                      memory_string, 
                      perl = TRUE)
  
  instructions <- regmatches(memory_string, matches)[[1]]
  
  # Track whether mul instructions are enabled (default: enabled)
  enabled <- TRUE
  total_sum <- 0
  
  # Process each instruction in order
  for (instruction in instructions) {
    if (grepl(do_pattern, instruction)) {
      # Enable future mul instructions
      enabled <- TRUE
    } else if (grepl(dont_pattern, instruction)) {
      # Disable future mul instructions
      enabled <- FALSE
    } else if (enabled && grepl(mul_pattern, instruction)) {
      # Process mul instructions only if enabled
      operands <- as.numeric(str_match(instruction, mul_pattern)[, 2:3])
      total_sum <- total_sum + prod(operands)
    }
  }
  
  return(total_sum)
}

## Test
test_sum_with_conditions <- process_memory_with_conditions(example_2)
print(test_sum_with_conditions)

## Result
total_sum_with_conditions <- process_memory_with_conditions(input)
print(total_sum_with_conditions)

