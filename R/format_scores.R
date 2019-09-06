# Format Scores for scorex Function
#
# This function takes a score variable and applies
# a format to it based on user input.
format_scores <- function(score,
                          cut_method = c("bins", "breaks", "percentiles"),
                          method_args = NULL,
                          exceptions = NULL,
                          custom_cut_fnc = NULL,
                          ...) {
  UseMethod("format_scores")
}

# For non numeric column types, we will just return the score.
#' @export
format_scores.default <- function(score,
                                  cut_method = c("bins", "breaks", "percentiles"),
                                  method_args = NULL,
                                  exceptions = NULL,
                                  custom_cut_fnc = NULL,
                                  ...) {
  warning("Non-numeric score vector specified, all unique levels will be used.",
          call. = FALSE)
  if (!is.null(custom_cut_fnc)) {
    score <- custom_cut_fnc(score, ...)
    if (any(is.na(score))) score <- addNA(score)
  } else {
    if (any(is.na(score))) score <- addNA(score)
    return(score)
  }
}

# For numeric fields
#' @export
format_scores.numeric <- function(score,
                                  cut_method = c("bins", "breaks", "percentiles"),
                                  method_args = NULL,
                                  exceptions = NULL,
                                  custom_cut_fnc = NULL,
                                  ...) {
  if (!is.null(custom_cut_fnc)) {
    score <- custom_cut_fnc(score)
    if (any(is.na(score))) score <- addNA(score)
  } else if (is.null(cut_method)) {
    NULL
  } else {
    # Get name of cut method to use
    cut_method <- match.arg(cut_method)
    if (is.null(method_args))
      stop("cut_method specific, but method_args is NULL.", call. = FALSE)
    # Apply cut method to the score
    score <- do.call(cut_method, list(score, method_args, exceptions))
  }
  if (any(is.na(score))) score <- addNA(score)
  return(score)
}

# set.seed(123)
# t1 <- sample(c(-10, -5, 300:900), size = 50000, replace = TRUE)
# table(format_scores(t1, cut_method = "breaks", method_args = c(300, 400, 500), exceptions = c(-10, -5)))
# table(format_scores(t1, cut_method = "percentiles", method_args = c(1, 10, 50), exceptions = c(-10, -5)))
# table(format_scores(t1, cut_method = "bin", method_args = 4, exceptions = c(-10, -5)))

# table(format_scores(t1))
# format_scores(c("poop", "poop"), score_breaks = NULL)
# format_scores(as.factor(c("poop", "poop")))

