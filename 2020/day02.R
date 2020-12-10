## Part one
# Read data, seperate min-max; letter; password
df_raw <- read.table("input02.txt", sep = " ")
# Split min and max rows
range <- stringr::str_split_fixed(df_raw$V1, "-", 2)
# Build df of min/max/letter/password
df <- data.frame(
  min = as.numeric(range[, 1]),
  max = as.numeric(range[, 2]),
  letter = gsub(":", "", df_raw$V2),
  pass = df_raw$V3
)

# number of times target letter appears in password
func <- function(x) {
  lengths(regmatches(x["pass"], gregexpr(x["letter"], x["pass"])))
}
count <- apply(df, 1, func)

# filter passwords where number of letters within range
poss_pass <- count >= df$min & count <= df$max
sum(poss_pass)

## Part two
# Extract password letter in "min" position
min_letter <- substr(df[, "pass"], df[, "min"], df[, "min"])
# Extract password letter in "max" position
max_letter <- substr(df[, "pass"], df[, "max"], df[, "max"])

# Check that letters in min/max position the same, and match designated letter
check <- (min_letter == df[, "letter"]) - (max_letter == df[, "letter"])
sum(check != 0)
