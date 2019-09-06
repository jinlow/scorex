#' scorex
#'
#' A dual score matrix function that allows for multiple
#' performance, and cross variables.
#' @export
scorex <- function(formula, data,
                   cut_method = c("bins", "breaks", "percentiles"),
                   method_args = NULL,
                   exceptions = NULL,
                   custom_cut_fnc = NULL, ...) {
  # Get data fields
  fd <- get_formula_fields(formula, data)

  if (missing(cut_method))
    cut_method <- NULL

  # Prep LHS and
  fd[1:2] <- lapply(fd[1:2], function(s)
    lapply(s, function(v)
      format_scores(v,
                    cut_method = cut_method,
                    method_args = method_args,
                    exceptions = exceptions,
                    custom_cut_fnc = custom_cut_fnc)))

  # Only keep non null items in list
  if(is.null(fd$RHS_R_vars)) fd$RHS_R_vars <- NULL

  return(fd)
}


#
# table(test$LHS_vars$score1, test$RHS_L_vars$score2)
# table(test)
# method_args = NULL,
# exceptions = NULL,
# custom_cut_fnc = NULL)
