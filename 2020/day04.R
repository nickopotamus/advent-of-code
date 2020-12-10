library(tidyverse)

## Turn data into a sensible format
input <- strsplit(readLines('input04.txt'), ' ')
# Separate into individual passports by finding empty lines
passport_id = cumsum(!lengths(input))
# Build key/value pairs
pairs <- lapply(strsplit(unlist(input), ':'),
                setNames,
                c('key', 'value'))
# Build tidy df of passports and key/value pairs
passports <- data.frame(id = rep(passport_id, lengths(input)),
                        do.call(rbind, pairs))

## Part 1
required <- c('byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid')
valid1 <- passports %>%
  group_by(id) %>%
  summarise(valid = !length(setdiff(required, key)))

sum(valid1$valid)

## Part 2
# Expand into wider format for fitering
passports2 <- passports %>%
  pivot_wider(names_from = key, values_from = value) %>%
  mutate(byr = as.integer(byr),
         iyr = as.integer(iyr),
         eyr = as.integer(eyr),
         hgt_value = as.numeric(gsub('cm|in$', '', hgt)),
         hgt_unit = gsub('\\d*', '', hgt))

# Filter for validity according to rules
valid2 <- passports2 %>%
  filter(byr >= 1920, byr <= 2002,
         iyr >= 2010, iyr <= 2020,
         eyr >= 2020, eyr <= 2030,
         hgt_value >= 150 & hgt_value <= 193 & hgt_unit == 'cm' |
           hgt_value >= 59 & hgt_value <= 76 & hgt_unit == 'in',
         grepl('^#[0-9a-f]{6}$', hcl),
         ecl %in% c('amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'),
         grepl('^\\d{9}$', pid))

nrow(valid2)
