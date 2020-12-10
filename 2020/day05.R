input <- readLines('input05.txt')

## Part 1
# Convert to input to binary - F/B and L/R are both 1/0 pairs
binary <- lapply(strsplit(input, ''), grepl, pattern = '[BR]')
# Calculate seat_id from binary
seat_id <- sapply(binary,
                  function(x) sum(x * 2^(rev(seq_along(x)) - 1)))
# Return maximum in the list
max(seat_id)

## Part 2
# Find any differencesdifference between the list of all possible seat_id's
# given min/max, and all included in the list
setdiff(seq(min(seat_id),max(seat_id)), # All seat_id's in order
        seat_id)                        # Actual list
