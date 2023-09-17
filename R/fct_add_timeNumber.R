#' add_timeNumber
#'
#' @description Add a time number (1, 2, ...) column to data including time points.
#'
#' @param data data with a time point column.
#'
#' @param tp_col_nm column name of the data column containing the time point information.
#'
#' @return data with the extra timeNumber column added
#'
#' @export

add_timeNumber <- function(data, tp_col_nm = "timestamp"){

  tp_vec <- data[, which(colnames(data) %in% tp_col_nm)]
  timestamp_un <- sort(unique(tp_vec))
  timeNb_lk <- 1:length(timestamp_un)
  names(timeNb_lk) <- timestamp_un
  data$timeNumber <- timeNb_lk[as.character(tp_vec)]

  return(data)

}
