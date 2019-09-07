# Base function for creating the frequency table of scores
table_freq <- function(scr1, scr2, exceptions, perf = NULL, func = NULL, idx_add = 0) {
  # Create the table
  if (is.null(perf)) tab <- table(scr1, scr2, useNA = "ifany")
  else {
    stopifnot(!is.null(func))
    tab <- tapply(X = perf, INDEX = list(scr1, scr2), FUN = func)
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
  return(tab)
}

score_table <- function(scr1, scr2, exceptions, ext_vars = NULL) {
  scr_x <- table_freq(scr1, scr2, exceptions = exceptions)

  ##### FIX WHAT IS HAPPENING WITH i
  ext_vars <- lapply(seq_along(ext_vars), function(ev) {
    rbind(table_freq(scr1, scr2,
                     exceptions = exceptions, perf = ext_vars[[ev]],
                     func = sum, idx_add = ev/10),
          table_freq(scr1, scr2,
                     exceptions = exceptions, perf = ext_vars[[ev]],
                     func = mean, idx_add = (ev/10) + 0.1))
  })

  scr_tab <- do.call(rbind, c(list(scr_x), ext_vars))
  scr_tab <- scr_tab[order(scr_tab$idx_col), ]
  scr_tab$idx_col <- NULL

  return(scr_tab)
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
