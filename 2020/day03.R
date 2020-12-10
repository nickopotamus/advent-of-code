# Read input - 323 routes
input <- readLines('input03.txt')

## Part 1
# Moves 3 right, 1 down; returns position on each of the 323 rows
# (Loops at end of row)
positions <- (3 * (seq_along(input) - 1)) %% nchar(input) + 1
# Boolean if tree is encountered, then sums all trues
sum(substr(input, positions, positions) == '#')


## Part 2
# Wrap above in a generic function
count_trees <- function(right, down = 1) {
  vertical <- seq(0, length(input) - 1, by = down) + 1
  horizontal <- (right * (seq_along(input) - 1)) %% nchar(input) + 1
  horizontal <- head(horizontal, length(vertical))
  as.double( # To avoid overflow in integers
    sum(substr(input[vertical], horizontal, horizontal) == '#')
  )
}
# Sum all the possible routes
total =
  count_trees(1) *
  count_trees(3) *
  count_trees(5) *
  count_trees(7) *
  count_trees(1, 2)
