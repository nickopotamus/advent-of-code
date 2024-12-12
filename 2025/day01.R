library(tidyverse)
library(cookiemonster)
library(httr2)
# add_cookies('.txt')

# Part 1 ####

## Example data
left_list <- c(3, 4, 2, 1, 3, 3)
right_list <- c(4, 3, 5, 3, 9, 3)

## Get input data
input <- request("https://adventofcode.com/2024/day/1/input") %>% 
  req_options(cookie = get_cookies("adventofcode.com", as = "string")) %>% 
  req_perform() %>% 
  resp_body_string() %>% 
  read_table(col_names = c("left", "right"))

## Function for part 1
calculate_total_distance <- function(left_data, right_data) {
  
  # Sort both lists
  sorted_left <- sort(left_data)
  sorted_right <- sort(right_data)
  
  # Calculate pairwise absolute differences
  distances <- abs(sorted_left - sorted_right)
  
  # Return the total distance
  sum(distances)
}

## Test
test_distance <- calculate_total_distance(left_list, right_list)
print(test_distance)

## Result
total_distance <- calculate_total_distance(input$left, input$right)
print(total_distance)

# Part 2 ####

## Function for part 2
calculate_similarity_score <- function(left_data, right_data) {
  
  # Count occurrences of each number in the right list
  right_counts <- right_data %>%
    as_tibble() %>%
    group_by(value) %>%
    summarise(count = n(), .groups = "drop")
  
  # Calculate the similarity score
  similarity_score <- left_data %>%
    as_tibble() %>%
    # inner_join so only returns left values with a count in right
    # could also left_join then convert NAs to 0
    inner_join(right_counts, by = c("value" = "value")) %>%
    mutate(score = value * count) %>% 
    summarise(total_score = sum(score)) %>%
    pull(total_score)
  
  return(similarity_score)
}

## Test
test_score <- calculate_similarity_score(left_list, right_list)
print(test_score)

## Result
similarity_score <- calculate_similarity_score(input$left, input$right)
print(similarity_score)
