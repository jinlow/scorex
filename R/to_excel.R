#' Scorex to Excel
#'
#' Output a Scorex object to Excel
#'
#' @param x An object of class \code{scorex}
#' @param row The row to start writing the tables to.
#'   Default is set to 1.
#' @param col The column to start writing the tables to.
#'   Default is set to 1.
#' @param wb A workbook object created with \code{\link[openxlsx]{createWorkbook}}.
#'   If no is workbook provided one will be created.
#' @param sheet A workbook worksheet name created with \code{\link[openxlsx]{addWorksheet}}.
#'   If no worksheet name is provided, a worksheet will be created.
#' @param open Should the workbook updated with the dual score tables be opened?
#'   Default is set to \code{TRUE}.
#' @export
scorex_to_xl <- function(x, row = 1, col = 1, wb = NULL, sheet = NULL, open = TRUE) {
  if (is.null(wb)) wb <- openxlsx::createWorkbook()
  if (is.null(sheet)) sheet <- "scorex tables"
  if (!(sheet %in% wb$sheet_names))
    openxlsx::addWorksheet(wb, sheetName = sheet, gridLines = FALSE)

  if (class(x) != "scorex") stop("x must be of class scorex")

  x <- x$tables

  header_style <- openxlsx::createStyle(fgFill = "#d3daea",
                                        halign = "CENTER",
                                        valign = "center",
                                        textDecoration = "Bold",
                                        border = "TopBottomLeftRight",
                                        borderColour = "black",
                                        fontColour = "black")

  for (i in seq_len(length(x))) {
    scorex_table_to_xl(x = x[[i]],
                       row = row,
                       col = col,
                       wb = wb,
                       sheet = sheet,
                       header_style = header_style)
    # Progress bar
    progr <- paste(rep("=", (20*i/length(x))), collapse="")
    cat(sprintf("\r%s : %-20s| %-50s", "Writing", progr, paste0("Scorex Table ", i)))
    # Update start_row in the parent enviroment.
    row <- (row + (nrow(x[[i]]) + (2)))
  }
  cat("\n")

  if (open) {
    openxlsx::openXL(wb)
  }

  return(invisible(wb))
}

scorex_table_to_xl <- function(x, row, col, wb, sheet, header_style) {
  openxlsx::writeData(wb = wb,
                      sheet = sheet,
                      x = x,
                      startCol = col,
                      startRow = row,
                      borders = "all",
                      headerStyle = header_style,
                      keepNA = FALSE)

  # Merge Cells
  mcells <- which(x[,1] != "") + row
  mcells_max <- mcells + (mcells[[2]] - mcells[[1]]) - 1
  mseqs <- mapply(seq, mcells, mcells_max, SIMPLIFY = FALSE)
  invisible_lapply(mseqs, function(msq) {
    openxlsx::mergeCells(wb = wb, sheet = sheet, cols = (col), rows = msq)
  })

  # Format Percent rows
  pct_style <- openxlsx::createStyle(numFmt = "PERCENTAGE",
                                     border = "TopBottomLeftRight")
  pct_rows <- grep(pattern = "Rate", x = x[,2], ignore.case = TRUE)
  openxlsx::addStyle(wb = wb,
                     sheet = sheet,
                     style = pct_style,
                     rows = (pct_rows + (row)),
                     cols = (col + 2):(ncol(x) + col - 1),
                     gridExpand = TRUE)

  # Add side formatting
  openxlsx::addStyle(wb = wb,
                     sheet = sheet,
                     style = header_style,
                     rows = row:(nrow(x) + row),
                     cols = col,
                     gridExpand = TRUE)

  # Add Totals Formatting
  tot_row <- grep(pattern = "^Totals$", x = x[,1])
  openxlsx::addStyle(wb = wb,
                     sheet = sheet,
                     style = header_style,
                     rows = (tot_row + row):(nrow(x) + row),
                     cols = (col + 1),
                     gridExpand = TRUE)

  # Format Remainder of Description
  side_style <- openxlsx::createStyle(fgFill = "#edf0ef",
                                        halign = "CENTER",
                                        textDecoration = "Bold",
                                        border = "TopBottomLeftRight",
                                        borderColour = "black",
                                        fontColour = "black")
  openxlsx::addStyle(wb = wb,
                     sheet = sheet,
                     style = side_style,
                     rows = (row + 1):(tot_row + row - 1),
                     cols = (col + 1),
                     gridExpand = TRUE)

  # openxlsx::addStyle(wb = wb,
  #                    sheet = sheet,
  #                    style = side_style,
  #                    rows = (tot_row + row):(nrow(x) + row),
  #                    cols = (col + 2):(col + ncol(x) - 1),
  #                    gridExpand = TRUE)

  # Col Width
  openxlsx::setColWidths(wb, sheet = sheet, cols = col:(ncol(x) + col),
                         widths = c(15, 10, rep(7.4, (ncol(x) - 2))))

  # Conditional Formatting
  cond_max <- max(x[pct_rows, 3:ncol(x)])
  cond_min <- min(x[pct_rows, 3:ncol(x)])
  invisible_lapply (pct_rows, function(.x) {
    openxlsx::conditionalFormatting(wb,
                                    sheet,
                                    cols = 3:nrow(x) + (col - 1),
                                    rows = (row + .x),
                                    rule = c(cond_min, mean(c(cond_max, cond_min)), cond_max),
                                    type = 'colorScale',
                                    style = c("#70c66f", "#ffe88c", "#ff6376"))})
}
