# Check if a value is in a range
# x: A single value
# y: a numeric vector
`%in-range%` <- function(x, y) {
  rng <- range(y)
  rng[[1]] <= x & x <= rng[[2]]
}

# Same as is.na.data.frame
is.nan.data.frame <- function(x) {
  do.call(cbind, lapply(x, is.nan))
}

#' S3 print method for scorex_list
#' @export
print.scorex <- function(x, ...) {
  scorex_summary <- sprintf("Scorex list called from:\n %s", deparse(x$formula))
  cat(scorex_summary)
}

# Lapply for side effects
invisible_lapply <- function(X, FUN, ...) {
  X <- lapply(X, FUN, ...)
  invisible(X)
}
