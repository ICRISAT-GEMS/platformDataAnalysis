#' plot_trend
#'
#' @description Plot the trend of a trait value.
#'
#' @param data data.frame with a "timeNb" column describing the different
#' timestep of the data (1, 2, ...), the trait and plotId column. Object
#' obtained with the function \code{\link{median_computation}}
#'
#' @param trait character string specifiying the trait.
#'
#' @param main Title of the graph.
#'
#' @param genotype logical value specifying if the grouping factor is genotype.
#' By default values are grouped by plot.
#'
#' @return The return value, if any, from executing the function.
#'
#' @export

plot_trend <- function(data, trait = NULL, main = NULL, genotype = FALSE){

  # select the trait
  # geno_pos <- which(colnames(data) == "genotype")
  # trait_nm <- colnames(data)[(geno_pos + 1):ncol(data)]
  #
  # if(!(trait %in% trait_nm)){
  #
  #   stop(paste("the 'trait' string is not present in the list of traits of data.",
  #              " Please use one of the following:", paste(trait_nm, collapse = ", ")))
  #
  # }

  # transform the time stamp into difference per day
  timeNumber <- yday(data$timePoint)
  data$timeNumber <- timeNumber - min(timeNumber, na.rm = TRUE) + 1

  colnames(data)[which(colnames(data) == trait)] <- "value"

  if(is.null(main)){main = paste(trait, "trend")}

  if(genotype){

    p <- ggplot(data = data, aes(x = timeNumber, y = value, group = genotype)) +
      geom_line() + ylab(trait) + ggtitle(main)

  } else {

    p <- ggplot(data = data, aes(x = timeNumber, y = value, group = plotId)) +
      geom_line() + ylab(trait) + ggtitle(main)

  }



  return(p)

}
