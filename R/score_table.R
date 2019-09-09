# Base function for creating the frequency table of scores
table_freq <- function(scr1, scr2,
                       exceptions,
                       perf = NULL,
                       idx_add = 0,
                       description = "N Obs") {
  # Create the table
  if (is.null(perf)) {
    tab <- tapply(rep(1, length(scr1)), INDEX = list(scr1, scr2), FUN = sum)
    tab[is.na(tab)] <- 0
  } else {
    tab <- tapply(X = perf, INDEX = list(scr1, scr2), FUN = sum)
    tab[is.na(tab)] <- 0
  }

  # Save Rownames
  rnms <- row.names(tab)
  rnms[is.na(rnms)] <- "Missing"

  # Save Col Names
  cnms <- colnames(tab)
  cnms[is.na(cnms)] <- "Missing"

  # Convert and fix Rownames and ColNames
  tab <- as.data.frame.matrix(tab)

  colnames(tab) <- cnms
  row.names(tab) <- NULL

  tab[,'crss_scr'] <- rnms
  tab <- tab[,c(ncol(tab), 1:(ncol(tab) - 1))]

  # Move Missing and exceptions
  w_excp_rows <- which(rnms %in% c(exceptions, "Missing"))
  w_excp_cols <- which(colnames(tab) %in% c(exceptions, "Missing"))

  tab <- tab[c(setdiff(seq_len(nrow(tab)), w_excp_rows), w_excp_rows),
             c(setdiff(seq_len(ncol(tab)), w_excp_cols), w_excp_cols)]

  tab$idx_col <- seq(nrow(tab)) + idx_add

  # Add Description column
  tab$Field <- description
  tab <- tab[, c(1, ncol(tab), 2:(ncol(tab) - 1))]

  # Add Totals Column
  tab <- rbind(tab,
               c(list("crss_scr" = "Totals", "Field" = "N Obs"),
                 colSums(tab[, - c(1, 2, ncol(tab))]),
                 "idx_col" = (nrow(tab) + 1)))

  tab$Totals <- rowSums(tab[, - c(1, 2, ncol(tab))])

  return(tab)
}

# Combine scores and multiple performances.
score_table <- function(scr1, scr2, exceptions, ext_vars = NULL, scr_names = c("score1", "score2")) {
  scr_x <- table_freq(scr1, scr2, exceptions = exceptions)

  ext_vars <- lapply(seq_along(ext_vars), function(ev) {
    var_sum <- table_freq(scr1, scr2,
                          exceptions = exceptions, perf = ext_vars[[ev]],
                          idx_add = ((ev/10) + (0.1 * (ev-1))),
                          description = sprintf("N %s", names(ext_vars)[[ev]]))
    # Create Rates
    var_rate <- var_sum
    # Remove round later
    var_rate[, -c(1, 2, ncol(var_rate) - 1)] <- round(var_rate[, -c(1, 2, ncol(var_rate) - 1)] /
                                                   scr_x[, -c(1, 2, ncol(scr_x) - 1)], 2)
    # Create Rate Columns
    var_rate[is.nan(var_rate)] <- 0
    var_rate$idx_col <- scr_x$idx_col + (ev/10) + (0.1 * ev)
    var_rate$Field <- sprintf("Rate %s", names(ext_vars)[[ev]])

    var_sum$crss_scr <- ""
    var_rate$crss_scr <- ""
    rbind(var_sum, var_rate)
  })

  scr_x <- do.call(rbind, c(list(scr_x), ext_vars))
  scr_x <- scr_x[order(scr_x$idx_col), ]
  scr_x$idx_col <- NULL
  names(scr_x)[[1]] <- sprintf("%s X %s", scr_names[[1]], scr_names[[2]])
  rownames(scr_x) <- NULL

  return(scr_x)
}


# set.seed(123)
# examp <- data.frame(score1 = sample(c(NA, -10, -5, 300:999), size = 500000, replace = TRUE),
#                     score2 = sample(c(rep(300:330, 5), 300:999, -5), size = 500000, replace = TRUE),
#                     perf   = sample(c(0, 0, 0, 1), size = 50000, replace = TRUE),
#                     bvar1  = sample(1:25, size = 50000, replace = TRUE))
#
# test <- scorex(score2 + score1 ~ score2 | perf, data = examp,
#                cut_method = "percentile",
#                method_args = c(1, 5, 15, 25, 50, 75), exceptions = c(-10, -5, NA))
#
#
# scorex:::table_freq(test$LHS_vars$score1, test$RHS_L_vars$score2, exceptions = c(-10, -5, NA))
# scorex:::table_freq(test$LHS_vars$score1, test$RHS_L_vars$score2, exceptions = c(-10, -5, NA),
#                     perf = test$RHS_R_vars$perf, func = sum)
#
# scorex:::table_freq(test$LHS_vars$score1, test$RHS_L_vars$score2, exceptions = c(-10, -5, NA),
#                     perf = test$RHS_R_vars$perf, func = mean)
