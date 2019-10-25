# Prepare the formula for the cross table function
# formula: the formula passed into the cross table.
#
# prep_formula(x + m + z ~ y + m)
prep_formula <- function(formula) {
  # Allow for formula to be a character string.
  formula <- formula(formula)

  LHS <- deparse(formula[[2]])

  # Check that LHS only has plus
  if (grepl("\\|", x = LHS, perl = TRUE)) {
    stop("Invalid character in left hand side of formula. Only '+' supported.", call. = FALSE)
  }

  # Split terms of LHS
  LHS <- strsplit(x = LHS, split = "\\s*\\+\\s*")[[1]]

  # Parse RHS
  RHS <- deparse(formula[[3]])

  # Split terms at '|'.
  RHS <- strsplit(x = RHS, split = "\\s*\\|\\s*")[[1]]

  # Check only a single '|' is used
  if (length(RHS) > 2) {
    stop("Right hand side of formula may only cantain single '|'", call. = FALSE)
  }

  RHS_L <- strsplit(x = RHS[[1]], split = "\\s*\\+\\s*")[[1]]

  # Gather Bivariates, and percent bivariates
  if (length(RHS) == 2) {
    RHS_R <- strsplit(x = RHS[[2]], split = "\\s*\\+\\s*")[[1]]
    if (any(grepl(pattern = "\\!", x = RHS_R, ignore.case = TRUE))) {
      RHS_R_bv <- grep(pattern = "\\!", x = RHS_R, ignore.case = TRUE, value = TRUE)
      RHS_R <- setdiff(RHS_R, RHS_R_bv)
      RHS_R_bv <- gsub(pattern = "\\!\\s*", replacement = "", x = RHS_R_bv)

      if (length(RHS_R) == 0) RHS_R <- NULL

    } else RHS_R_bv <- NULL
  } else {
    RHS_R <- NULL
    RHS_R_bv <- NULL
  }

  return(list(LHS = LHS, RHS_L = RHS_L, RHS_R = RHS_R, RHS_R_bv = RHS_R_bv))
}

# Get Field
# Get a single field in the form returned by the
# prep_formula field
get_side_fields <- function(formula_side, x) {
  side_vars <- lapply(formula_side, function(v) {
    eval(parse(text = v), envir = x, enclos = parent.frame())})
  names(side_vars) <- formula_side
  return(side_vars)
}

# Get the data from the formula for the table
# formula: a formula passed into the cross table function.
# x: the data used for the cross table
#
# test <- get_formula_fields(mpg + cyl ~ disp + hp | wt + qsec, x = mtcars)
get_formula_fields <- function(formula, x) {
  formula_list <- prep_formula(formula)

  # Get LHS variables
  LHS_vars <- get_side_fields(formula_list[[1]], x)

  # RHS_L Vars
  RHS_L_vars <- get_side_fields(formula_list[[2]], x)

  # RHS_R if any
  if (!is.null(formula_list[[3]])) {
    RHS_R_vars <- get_side_fields(formula_list[[3]], x)
  } else {
    RHS_R_vars <- NULL
  }

  # RHS_R_bv if any
  if (!is.null(formula_list[[4]])) {
    RHS_R_bvs <- get_side_fields(formula_list[[4]], x)
  } else {
    RHS_R_bvs <- NULL
  }

  return(list(LHS_vars = LHS_vars,
              RHS_L_vars = RHS_L_vars,
              RHS_R_vars = RHS_R_vars,
              RHS_R_bvs = RHS_R_bvs))
}
