#' scorex
#'
#' A dual score matrix function that allows for multiple
#' performance, and cross variables.
#'
#' @param formula A formula of the form LHS ~ RHS to use to specify which fields
#'   to use as the scores and performance measures in the tables, see Details.
#' @param data A data.frame, that has the fields specified in the formula.
#' @param cut_method The method to use to format the score in the final table.
#'   Currently the available methods are:
#'   \itemize{
#'     \item \code{bins}: Break the scores into a set number of even bins.
#'     \item \code{breaks}: Set the score values to break the score at.
#'     \item \code{percentiles}: Set the percentiles to use to break the scores at.
#'   }
#' @param method_args Arguments to use for the method specified in \code{cut_method}
#'   each method uses the following argument.
#'   \itemize{
#'     \item \code{bins}: Provide a integer specifying the even number of breaks to create.
#'     \item \code{breaks}: Provide a vector that specifies the cuts to use.
#'     \item \code{percentiles}: Provide a vector of percentiles between the values of 0 and 1.
#'   }
#' @param exceptions A vector of exception values to exclude from the cross table.
#'   The default is set to \code{NA}.
#' @param custom_cut_fnc A custom function to use to cut the scores.
#' @param ... Other arguments to pass to the \code{custom_cut_fnc}
#' @details
#' Fill out...
#' @export
scorex <- function(formula, data,
                   cut_method = c("bins", "breaks", "percentiles"),
                   method_args = NULL,
                   exceptions = NA,
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
                    custom_cut_fnc = custom_cut_fnc,
                    ...)))

  # Only keep non null items in list
  if(is.null(fd$RHS_R_vars)) fd$RHS_R_vars <- NULL

  # Create Score Tables
  scr_crss_tabs <- lapply(names(fd$LHS_vars), function(s1_nm) {
    lapply(names(fd$RHS_L_vars), function(s2_nm) {
      score_table(fd$LHS_vars[[s1_nm]], fd$RHS_L_vars[[s2_nm]], exceptions = exceptions,
                  ext_vars = fd$RHS_R_vars,
                  scr_names = c(s1_nm, s2_nm))
    })
  })


  return(scr_crss_tabs)
}


#
# table(test$LHS_vars$score1, test$RHS_L_vars$score2)
# table(test)
# method_args = NULL,
# exceptions = NULL,
# custom_cut_fnc = NULL)
