#' convert_old_unit_row_col
#'
#' @description Convert the old unit information (A-1-1) into row column info
#' A-1 (first column), ...-...- 1 (first row or sector).
#'
#' @param x vector of 'old unit' LeasyScan plot positions
#'
#' @return data.frame with row and column position
#'
#' @export

convert_old_unit_row_col <- function(x){

  # lookup to convert columns
  col_conv_lk <- 1:16
  names(col_conv_lk) <- paste0(rep(LETTERS[1:8], each = 2), rep(1:2, 8))


  # split the row and column information
  x_split <- strsplit(x = x, split = "-")

  # column
  x_col1 <- unlist(lapply(X = x_split, `[[`, 1))
  x_col2 <- unlist(lapply(X = x_split, `[[`, 2))
  x_col <- paste0(x_col1, x_col2)
  x_col <- as.numeric(col_conv_lk[x_col])

  # row
  x_row <- as.numeric(unlist(lapply(X = x_split, `[[`, 3)))

  return(data.frame(rowNum = x_row, colNum = x_col))

}
