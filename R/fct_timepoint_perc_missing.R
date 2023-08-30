#' timepoint_prop_non_missing
#'
#' @description Determine the proportion of plot with at least one observation
#' # for the set of trait over the time points of the time series.
#'
#' @param data data.frame with a "timePoint" column describing the different
#' timestep of the data, the experimental design information like replicate,
#' block, row, columns, etc. This part finish with the "genotype" column indicating
#' the genotype treatment of the experiment. Then all remaining column are
#' numeric trait values.
#'
#' @return vector of proportion of non missing sector per day.
#'
#' @export

timepoint_prop_non_missing <- function(data){

  tp_id <- sort(unique(data$timePoint))
  plot_id <- unique(data$plotId)
  n_plot <- length(plot_id)

  geno_col <- which(colnames(data) == "genotype")

  prop_non_missing <- rep(NA, length(tp_id))

  for(i in 1:length(tp_id)){

    d_i <- data[data$timePoint == tp_id[i], (geno_col + 1):ncol(data)]
    non_missing_rows <- sum(apply(d_i, 1, function(row) any(!is.na(row))))

    prop_non_missing[i] <- non_missing_rows / n_plot

  }

  names(prop_non_missing) <- tp_id
  return(prop_non_missing)

}
