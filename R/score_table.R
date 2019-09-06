# Base function for creating the frequency table of scores
table_scorfreq <- function(scr1, scr2) {
  tab <- as.data.frame.matrix(table(scr1, scr2, useNA = "ifany"))
  tab[,'crss_scr'] <- row.names(tab)
  row.names(tab) <- NULL
  tab <- tab[,c(ncol(tab), 1:(ncol(tab) - 1))]
  tab$idx_col <- seq(nrow(tab))
  return(tab)
}

# Function for creating frequency table of perf by score
table_perffreq <- function(scr1, scr2, perf, func = sum) {
  labs1 <- levels(scr1)
  labs2 <- levels(scr2)

  scr_comb <- paste(scr1, scr2, sep = "|^|")
  dat <- as.numeric(tapply(perf, scr_comb, func, simplify = FALSE))
  tab <- matrix(dat, nrow = length(labs1), dimnames = list(labs1, labs2), byrow = TRUE)
  tab <- as.data.frame.matrix(tab)
  tab[,'crss_scr'] <- row.names(tab)
  row.names(tab) <- NULL
  tab <- tab[,c(ncol(tab), 1:(ncol(tab) - 1))]
  tab$idx_col <- seq(nrow(tab))
  return(tab)
}

score_table <- function(scr1, scr2, ext_vars) {

}
