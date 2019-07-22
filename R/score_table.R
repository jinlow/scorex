table_scr <- function(scr1, scr2) {
  labs1 <- levels(scr1)
  labs2 <- levels(scr2)

  # Create Label information
  scr_comb <- paste(scr1, scr2, sep = "+:+")
  scr_comb_labs <- expand.grid(labs1, labs2)
  scr_comb_labs <- paste(scr_comb_labs[,1], scr_comb_labs[,2], sep = "+:+")

  # Create the cross table
  scr_mat <- factor(scr_comb, levels = scr_comb_labs)
  scr_mat <- tabulate(scr_mat)
  scr_mat <- array(scr_mat, dim = c(6, 4), dimnames = list(labs1, labs2))

  return(scr_mat)
}

# This is still slower than table()
