library(tidyverse)
input <- readLines('input06.txt')

## Part 1
# Assign each line to a group (divided by empyty line)
group <- cumsum(!nchar(input))
# tidy tibble of answers by group
responses <- tibble(group = group[nchar(input) > 0],
                        answers = input[nchar(input) > 0])
# count number of unique letters in each group i.e. union
unique_letters <- responses %>%
  mutate(answers = strsplit(answers, '')) %>%
  group_by(group) %>%
  summarise(count = Reduce(union, answers) %>% length)
  # or summarise(count = length(unique(unlist(strsplit(answers, '')))))
sum(unique_letters$count)

## Part 2
# Same, but interesation rather than union
identical_letters <- responses %>%
  mutate(answers = strsplit(answers, '')) %>%
  group_by(group) %>%
  summarise(count = Reduce(intersect, questions) %>% length)
sum(identical_letters$count)
