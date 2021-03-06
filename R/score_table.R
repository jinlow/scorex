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
               c(list("crss_scr" = "Totals", "Field" = description),
                 colSums(tab[, - c(1, 2, ncol(tab))]),
                 "idx_col" = (nrow(tab) + 1 + idx_add)))

  tab$Totals <- rowSums(tab[, - c(1, 2, ncol(tab))])

  return(tab)
}

# Combine scores and multiple performances.
score_table <- function(scr1, scr2,
                        exceptions,
                        ext_vars = NULL, ext_bv_vars = NULL,
                        scr_names = c("score1", "score2")) {
  scr_x <- table_freq(scr1, scr2, exceptions = exceptions)

  # TODO: Refactor code
  # Combine extra vars if bvars present
  if (!is.null(ext_bv_vars)) {
    ext_bv_vars <- lapply(ext_bv_vars, function(x) {
      attr(x, "bvs") <- TRUE
      x})
    ext_vars <- c(ext_vars, ext_bv_vars)
  }

  ext_vars <- lapply(seq_along(ext_vars), function(ev) {

    # Edit Description, and mean or rate
    if (!is.null(attributes(ext_vars[[ev]])$bvs)) {
      rate_desc <- "Mean"
      tab_description <- sprintf("Sum %s", names(ext_vars)[[ev]])
    } else {
      rate_desc <- "Rate"
      tab_description <- sprintf("N %s", names(ext_vars)[[ev]])
    }

    var_sum <- table_freq(scr1, scr2,
                          exceptions = exceptions, perf = ext_vars[[ev]],
                          idx_add = ((ev/10) + (0.1 * (ev-1))),
                          description = tab_description)
    # Create Rates
    var_rate <- var_sum
    # Remove round later
    var_rate[, -c(1, 2, ncol(var_rate) - 1)] <- var_rate[, -c(1, 2, ncol(var_rate) - 1)] /
                                                   scr_x[, -c(1, 2, ncol(scr_x) - 1)]
    # Create Rate Columns
    var_rate[is.nan(var_rate)] <- 0
    var_rate$idx_col <- scr_x$idx_col + (ev/10) + (0.1 * ev)

    var_rate$Field <- sprintf("%s %s", rate_desc,names(ext_vars)[[ev]])

    var_sum$crss_scr <- ""
    var_rate$crss_scr <- ""
    rbind(var_sum, var_rate)
  })

  scr_x <- do.call(rbind, c(list(scr_x), ext_vars))
  scr_x <- scr_x[order(scr_x$idx_col), ]
  scr_x$idx_col <- NULL
  names(scr_x)[[1]] <- sprintf("%s X %s", scr_names[[1]], scr_names[[2]])
  rownames(scr_x) <- NULL

  class(scr_x) <- c("scorex_table", class(scr_x))

  return(scr_x)
}
