# Base function for creating the frequency table of scores
table_freq <- function(scr1, scr2, exceptions, perf = NULL, func = NULL) {
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

  tab$idx_col <- seq(nrow(tab))
  return(tab)
}

score_table <- function(scr1, scr2, exceptions, ext_vars = NULL) {

}
