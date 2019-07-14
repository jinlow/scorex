# Check if a value is in a range
# x: A single value
# y: a numeric vector
`%in-range%` <- function(x, y) {
  rng <- range(y)
  rng[[1]] <= x & x <= rng[[2]]
}
