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

#' S3 print method for scorex_table
#' @export
print.scorex_table <- function(x, ...) {
  x <- as.data.frame(x)
  x[, c(-1,-2)] <- lapply(x[, c(-1,-2)], round, 2)
  print(x)
}

# Lapply for side effects
invisible_lapply <- function(X, FUN, ...) {
  X <- lapply(X, FUN, ...)
  invisible(X)
}

# Get the number of decimal places
decimal_place <- function(x) {
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}
