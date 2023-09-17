#' extract_trait_data_adj_TS
#'
#' @description Etract trait value from a long format adjusted phenotype data frame
#' and return them in a wide format.
#'
#' @param data data.frame containing adjusted traits after the fitting of
#' spatial_adjustment function.
#'
#' @param tp_sel numeric vector of selected timepoints.
#'
#' @param trait_name character string indicating the name of the selected trait
#'
#' @param new_name optional new name of the selected trait.
#'
#' @param keep column from data that should be kept.
#'
#' @return The return value, if any, from executing the function.
#'
#' @export

extract_trait_data_adj_TS <- function(data, tp_sel, trait_name,
                                      new_name = NULL, keep = "genotype"){

  if(is.null(new_name)){
    new_name <- trait_name
  }

  data <- data[data$timeNumber %in% tp_sel, c("timePoint", keep, trait_name)]
  colnames(data)[ncol(data)] <- "trait"
  d_wide <-pivot_wider(data = data, names_from = timePoint, values_from = trait)
  colnames(d_wide) <- c(keep, paste0(new_name, "_t", tp_sel))

  return(d_wide)

}
