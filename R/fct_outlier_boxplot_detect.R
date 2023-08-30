#' outlier_boxplot_detect
#'
#' @description Detececion of the outliers using boxplot method.
#' All data above 1.5 third quartile and below 1.5 first quartile
#' are considered as outliers and set as missing.
#'
#' @param data data.frame with a "timePoint" column describing the different
#' timestep of the data, the experimental design information like replicate,
#' block, row, columns, etc. This part finish with the "genotype" column indicating
#' the genotype treatment of the experiment. Then all remaining column are
#' numeric trait values.
#'
#' @return data with outliers removed
#'
#' @export

outlier_boxplot_detect <- function(data){

  data <- data %>% arrange(timePoint)

  # double loop over the trait and days
  tp_id <- unique(data$timePoint)
  tp_id <- tp_id[!is.na(tp_id)]

  geno_col <- which(colnames(data) == "genotype")
  last_col <- ncol(data)

  for(i in (geno_col + 1):last_col){

    for(j in 1:length(tp_id)){

      pos_ij <- which(data$timePoint == tp_id[j])
      x_ij <- unlist(data[pos_ij, i])
      ol <- boxplot(x_ij, plot = FALSE)$out
      data[pos_ij[x_ij %in% ol], i] <- NA

    }

  }

  data <- data %>% arrange(rowNum, colNum, timePoint)

  return(data)

}
