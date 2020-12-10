## Part 1
library(tidyverse)
library(tidystringdist)

expenses <- read.delim( "input01.txt",  header = FALSE )

tidy_comb_all(expenses$V1) %>%  # Build all combinations
  mutate(sum = V1 + V2) %>%     # Adding them together
  filter(sum == 2020) %>%       # keeping only the one that equals 2020
  mutate(product = V1 * V2) %>% # Multiplying it
  pull(product)                 # Getting the answer

# Alternative (one-line) solution
expenses2 <- as.integer(readLines("input01.txt"))
prod(expenses[(2020 - expenses) %in% expenses])

## Part 2
combn(expenses$V1, 3) %>%
  # Transposing the matrix and turning it to a tibble
  t() %>% as_tibble() %>%
  mutate(sum = V1 + V2 + V3) %>%
  filter(sum == 2020)  %>%
  mutate(product = V1 * V2 * V3) %>%
  pull(product)

# Alternative
matches <- list()
for (i in 2020 - unlist(expenses2)) {matches[[i]] <- expenses2[(i - expenses2) %in% expenses2]}
prod(unique(unlist(matches)))

