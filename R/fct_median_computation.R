#' median_computation
#'
#' @description function to calculate the median data of each plot (sector)
#' for a certain number of traits
#'
#' @param data data.frame with a "timePoint" column describing the different
#' timestep of the data, the experimental design information like replicate,
#' block, row, columns, etc. This part finish with the "genotype" column indicating
#' the genotype treatment of the experiment. Then all remaining column are
#' numeric trait values.
#'
#' @return data.frame with all design element columns reduced and all the traits
#' with median values per day
#'
#' @export

median_computation <- function(data){

  # apply median computation over the last columns genotype -> last.
  # keep the rest of the information

  median_mdf <- function(x){
    x <- x[!is.na(x)]
    if(length(x) == 0) {NA} else {median(x = x)}
  }

  geno_col <- which(colnames(data) == "genotype")
  last_col <- ncol(data)-2

  meta_data <- data[, 1:geno_col]
  meta_data <- meta_data %>% group_by(plotId) %>%
    reframe(across(1:genotype, unique))

  result <- data %>%
    group_by(timePoint, plotId) %>%
    summarise(across((geno_col - 1):last_col, median_mdf))

  result <- left_join(x = result, y =  meta_data,
                      by = join_by("timePoint", "plotId"))
  result <- result[, colnames(data)]
  result <- result %>% arrange(rowNum, colNum, timePoint)


  return(result)

}
