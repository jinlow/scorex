# Functions to be specifed with the cut_method
# argument in the scorex function. Each function
# returns the score as an ordered factor.

# Break score into even bins
bins <- function(score, bins, exceptions = NULL) {
  if (length(bins) > 1) {
    stop("bins must be a single integer value.")
  }

  breaks <- quantile(score[!(score %in% exceptions)], probs = seq(0, 1, by = 1/bins))
  # If all scores are integers, round.
  if (all(score[!(score %in% exceptions)] %% 1 == 0)) breaks <- round(breaks, 0)

  score <- exc_cut(score, breaks, exceptions)
  return(score)
}

# table(bins(t1, 5,  exceptions = c(-10, -5)))

# Break score by specified score breaks
breaks <- function(score, breaks, exceptions = NULL) {
  score <- exc_cut(score, breaks, exceptions)
  return(score)
}

# table(breaks(t1, breaks = c(100, 500, 900), exceptions = c(-10, -5)))

# Break score by specified percentiles
percentiles <- function(score, percentiles, exceptions) {
  # If whole number are used for percentiles, convert to decimal
  if (any(percentiles > 1)) percentiles <- percentiles/100

  # Make sure the ends are considered [0,1]
  percentiles <- unique(c(0, 1, percentiles))
  breaks <- quantile(score[!(score %in% exceptions)], probs = percentiles)

  # If all scores are integers, round.
  if (all(score[!(score %in% exceptions)] %% 1 == 0)) breaks <- round(breaks, 0)

  score <- exc_cut(score, breaks, exceptions)
  return(score)
}

# table(percentiles(t1, percentiles = c(1, 5, 10, 50), exceptions = c(-5, -10)))

# Cut function that allows for exception values
exc_cut <- function (x, breaks, exceptions = NULL) {
  # Add min max to breaks if not present
  x_m_ex <- x[!(x %in% exceptions)] # Drop any exceptions when considering this
  if (!(min(x_m_ex) >= min(breaks))) breaks <- c(min(x_m_ex), breaks)
  if (!(max(x_m_ex) <= max(breaks))) breaks <- c(breaks, max(x_m_ex))

  breaks <- sort(breaks)
  broken <- .bincode(x, breaks, include.lowest = TRUE)

  labs <- make_labels(breaks)

  levs <- seq_along(labs)
  # Treat breaks if exception values are present
  if (!is.null(exceptions)) {
    exceptions <- sort(exceptions)
    # Check if exceptions in break values
    if (any(exceptions %in% breaks))
      stop("Exception values present in breaks.", call. = FALSE)
    # Drop Exceptions if not in X
    exceptions <- exceptions[exceptions %in% x]

    # If there is an exception that is one of the levs, this will
    # mess up the creation of a factor, for example 1.
    # To combat this we need to add to our levels.
    # If an exception is a level, we could add the highest
    # exception value to our levels, and broken and this
    # should get rid of the issue.
    if (any(exceptions %in% levs)) {
      levs <- levs + max(exceptions)
      broken <- broken + max(exceptions)
    }

    invisible(vapply(exceptions, function(excp) {
      excp_idx <- which(x == excp)
      broken[excp_idx] <<- excp
      return(1)
    }, FUN.VALUE = numeric(1)))

    labs <- c(as.character(exceptions), labs)
    levs <- c(exceptions, levs)
  }
  cut_score <- factor(x = broken,
                      levels = levs,
                      labels = labs,
                      ordered = TRUE)

  return(droplevels(cut_score))
}

# Make labels. This function expects the breaks are sorted.
make_labels <- function(breaks, exceptions = NULL) {
  brks <- breaks
  to_add <- vapply(breaks[-1], function(x) {
    if ((x %% 1 == 0)) return(1)
    else {
      div <- decimal_place(x)
      dec <- 1/(10^div)
      return(dec)}
    }, FUN.VALUE = numeric(1))

  # Create breaks
  breaks[-1] <- breaks[-1] + to_add
  labs <- paste0(breaks, "-", brks[-1])
  labs[-length(labs)]
}
