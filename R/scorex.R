#' scorex
#'
#' A dual score matrix function that allows for multiple
#' performance, and cross variables.
#'
#' @param formula A formula of the form LHS ~ RHS to use to specify which fields
#'   to use as the scores and performance measures in the tables, see Details.
#' @param data A data.frame, that has the fields specified in the formula.
#' @param cut_method The method to use to format the score in the final table.
#'   The method may be a single charcter specifying the cut method, or a list with
#'   two elements, specifing the method for the left, and right side of the formula
#'   respectively.
#'   Currently the available methods are:
#'   \itemize{
#'     \item \code{bins}: Break the scores into a set number of even bins.
#'     \item \code{breaks}: Set the score values to break the score at.
#'     \item \code{percentiles}: Set the percentiles to use to break the scores at.
#'   }
#' @param method_args Arguments to use for the method specified in \code{cut_method}
#'   each method uses the following argument.
#'   If \code{cut_method} is a list of two methods, \code{method_args} must also be a
#'   list of two elements, specifying the method arguments for the left and right side
#'   of the formula respectively.
#'   \itemize{
#'     \item \code{bins}: Provide a integer specifying the even number of breaks to create.
#'     \item \code{breaks}: Provide an integer vector that specifies the cuts to use.
#'     \item \code{percentiles}: Provide a vector of percentiles between the values of 0 and 1.
#'   }
#' @param exceptions A vector of exception values to exclude from the cross table.
#'   The default is set to \code{NA}.
#' @param custom_cut_fnc A custom function to use to cut the scores.
#' @param ... Other arguments to pass to the \code{custom_cut_fnc}
#' @details
#' \strong{Formula...}
#'
#'   The formula interface of scorex provides an API for intuatively creating dual score matrices.
#'   The formula takes the following form:
#'
#'   \strong{\code{LHS ~ RHS | extra vars}}
#'
#'   Each portion of the formula allows for the following funtionality.
#'   \describe{
#'       \item{LHS}{This portion of the formula can be used to identify one or several
#'       score like variables, each seperated with a \code{+}, that will be displayed along
#'       the right most column of the scorex table.}
#'       \item{RHS}{This portion of the formula can be used to identify one or several
#'       score like variables, each seperated with a \code{+}, that will be displayed along
#'       the first row of the scorex table.}
#'       \item{extra vars}{Optional variables to distribute along the score variables.
#'       By default these variables will be treated as binary performance fields, and the
#'       rate and total will be displayed. A \code{!} symbol (exclaimation mark) may be
#'       used to signify that the variable should be treated like a continuous numeric field
#'       such as a dollar value.}
#'            }
#'    If more than one variable is placed in the `LHS` or `RHS` of the formula
#'    dual score tables will be created of all possible combinations of the cross variables.
#'
#' @return
#' An object of class \code{scorex}.
#'
#' @examples
#' # Formula Examples
#' mc_tabs <- scorex(disp ~ hp, data = mtcars)
#' mc_tabs$tables
#'
#' mc_tabs <- scorex(disp ~ hp | vs, data = mtcars)
#' mc_tabs$tables
#'
#' mc_tabs <- scorex(disp ~ hp + mpg | vs, data = mtcars)
#' mc_tabs$tables
#'
#' mc_tabs <- scorex(disp ~ hp + mpg | vs + !wt, data = mtcars)
#' mc_tabs$tables
#'
#' # Examples of cut method
#' mc_tabs <- scorex(disp ~ hp | vs, data = mtcars,
#'                   cut_method = "bins", method_args = 4)
#' mc_tabs$tables
#'
#' mc_tabs <- scorex(disp ~ hp | vs, data = mtcars,
#'                   cut_method = list("bins", "percentiles"),
#'                   method_args = list(2, c(1, 25, 75, 100)))
#' mc_tabs$tables
#'
#'
#' @export
scorex <- function(formula, data,
                   cut_method = c("bins", "breaks", "percentiles"),
                   method_args = NULL,
                   exceptions = NA,
                   custom_cut_fnc = NULL, ...) {

  # Add NA to Exceptions
  if (!any(is.na(exceptions))) exceptions <- c(exceptions, NA)

  # Get data fields
  fd <- get_formula_fields(formula, data)

  if (missing(cut_method))
    cut_method <- NULL

  # Deal with list_cut_method
  # Prep LHS and
  fd[1:2] <- apply_score_format(fd = fd[1:2],
                                cut_method = cut_method,
                                method_args = method_args,
                                exceptions = exceptions,
                                custom_cut_fnc = custom_cut_fnc,
                                ...)

  # Only keep non null items in list
  if(is.null(fd$RHS_R_vars)) fd$RHS_R_vars <- NULL
  if(is.null(fd$RHS_R_bvs)) fd$RHS_R_bvs <- NULL

  # Create Score Tables
  scr_crss_tabs <- lapply(names(fd$LHS_vars), function(s1_nm) {
    lapply(names(fd$RHS_L_vars), function(s2_nm) {
      score_table(fd$LHS_vars[[s1_nm]], fd$RHS_L_vars[[s2_nm]], exceptions = exceptions,
                  ext_vars = fd$RHS_R_vars,
                  ext_bv_vars = fd$RHS_R_bvs,
                  scr_names = c(s1_nm, s2_nm))
    })
  })

  structure(
    list(
      tables = unlist(scr_crss_tabs, recursive = FALSE),
      formula = formula),
    class = "scorex")
}

# Internal ----
apply_score_format <- function(fd,
                               cut_method,
                               method_args,
                               exceptions,
                               custom_cut_fnc,
                               ...) {
  if (is.list(cut_method)) {
    stopifnot(class(cut_method) == class(method_args))
    if (!(length(cut_method) == 2 & length(method_args) == 2)) {
      stop("if cut_method and method_args are lists, they must both have two elements.")
    }
    fd[[1]] <- lapply(fd[[1]], function(v) format_scores(v,
                                                         cut_method = cut_method[[1]],
                                                         method_args = method_args[[1]],
                                                         exceptions = exceptions,
                                                         custom_cut_fnc = custom_cut_fnc,
                                                         ...))
    fd[[2]] <- lapply(fd[[2]], function(v) format_scores(v,
                                                         cut_method = cut_method[[2]],
                                                         method_args = method_args[[2]],
                                                         exceptions = exceptions,
                                                         custom_cut_fnc = custom_cut_fnc,
                                                         ...))
  } else if (is.character(cut_method) | is.null(cut_method)) {
    fd[1:2] <- lapply(fd[1:2], function(s)
      lapply(s, function(v)
        format_scores(v,
                      cut_method = cut_method,
                      method_args = method_args,
                      exceptions = exceptions,
                      custom_cut_fnc = custom_cut_fnc,
                      ...)))
  } else # if (!(is.null(cut_method) | is.null(method_args)))
    stop("cut_method and method_args must both be a list or a character.")
  return(fd)
}
