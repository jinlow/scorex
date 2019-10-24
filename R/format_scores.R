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
  } else if (is.null(cut_method)) {
    warning("cut_method not specified, score breaks will defualt to 5 even bins.")
    score <- bins(score, bins = 5, exceptions)
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
