#' spatial_adjustment
#'
#' @description Function performing spatial adjustment for a list of traits data
#' stored in a TP object. The adjustment is calculated for each trait x timepoint.
#' The function try to fit model from the most complex to the most simple:
#' a) (if fixed effect specified) fixed effect + spatial correction; b)
#' (if previous fail) spatial correction only; c) (if everything fail)
#' simple mean over genotype or row values for plot level. The simple average
#' is not performed if the genotype term is specified as random because the
#' difference of range between term averaged and term adjusted from a random term
#' can be too large. The single model computation is done using the util function
#' spatial_adjustment_ij.
#'
#' @param TP object of class TP. see documentation from statgenHTP package.
#'
#' @param traits vector of traits. must have the same identifiers as in the TP
#' object.
#'
#' @param timePoints numeric vector of time points for which the adjustment
#' should be performed.
#'
#' @param extraFixedFactors character vector specifying the term of the design
#' that should be included in the adjustment.
#'
#' @param geno.decomp see fitModels function from the statGenHTP package.
#'
#' @param what character string specifying if the genotype term should be fitted
#' as random or as fixed.
#'
#' @param useCheck logical value specifying if check term should be included
#' in the model. For more detail see fitModels function from the statGenHTP package.
#'
#' @param useRepId logical value. see fitModels function from the statGenHTP package.
#'
#' @param engine specify if SpATS or asreml package should be used for the adjustment.
#'
#' @param spatial see fitModels function from the statGenHTP package.
#'
#' @param quiet logical value specifying if the timepoint should be printed
#'
#' @return list with three data frame containing the: a) adjusted plot data;
#' b) the adjusted genotype data; c) the way the adjustment was performed
#'
#' @import SpATS
#' @import lubridate
#' @import ggplot2
#' @import tidyr
#'
#' @export

spatial_adjustment <- function(TP, traits, timePoints = NULL,
                               extraFixedFactors = NULL,
                               geno.decomp = NULL,
                               what = c("random", "fixed"),
                               useCheck = FALSE,
                               useRepId = FALSE,
                               engine = c("SpATS", "asreml"),
                               spatial = FALSE,
                               quiet = FALSE) {

  # loop over the traits and the timepoints
  n_traits <- length(traits)
  if(is.null(timePoints)){tp_vec <- 1:length(TP) } else {tp_vec <- timePoints}
  n_tp <- length(tp_vec)

  # reference matrix to monitor the computation
  comp_monitor <- matrix(NA, nrow =  n_traits, ncol = n_tp)
  rownames(comp_monitor) <- traits
  colnames(comp_monitor) <- names(TP)[tp_vec]

  # reference plot and geno data.frame

  d_TP <- do.call(what = rbind, args = TP)
  geno_pos <- which(colnames(d_TP) == 'genotype')
  plot_res_ref <- d_TP[, 1:geno_pos]

  tnb_vec <- 1:length(TP)
  tp_un <- names(TP)
  geno_vec <- unique(plot_res_ref$genotype)
  n_geno <- length(geno_vec)

  geno_res_ref <- data.frame(timeNumber = rep(tnb_vec, each = n_geno),
                             timePoint = rep(tp_un, each = n_geno),
                             genotype = rep(geno_vec, length(TP)))

  if(!is.null(timePoints)){
    geno_res_ref <- geno_res_ref[geno_res_ref$timeNumber %in% timePoints, ]
    plot_res_ref <- plot_res_ref[plot_res_ref$timeNumber %in% timePoints, ]
  }

  for(i in 1:n_traits){

    plot_res_ij <- vector(mode = "list", length = n_tp)
    geno_res_ij <- vector(mode = "list", length = n_tp)

    # specific case for TR
    if(traits[i] == "TR_VPD_slope"){ tp_vec_i <- tp_vec[1]
    } else { tp_vec_i <- tp_vec}


    for(j in tp_vec_i){

      # function that calculate the adjustment using: surface + fix, surface, or no correction

      tr_adj_ij <- spatial_adjustment_ij(TP = TP, tr_id = traits[i], tp = j,
                                         extraFixedFactors = extraFixedFactors,
                                         geno.decomp = geno.decomp, what = what,
                                         useCheck = useCheck, useRepId = useRepId,
                                         engine = engine, spatial = spatial, quiet = quiet)

      plot_res_ij[[j]] <- tr_adj_ij$plot_adj
      geno_res_ij[[j]] <- tr_adj_ij$geno_adj
      comp_monitor[i, j] <- tr_adj_ij$adj_type

    } # end loop over timepoints

    # add those results to the reference df
    plot_res_i <- do.call(what = rbind, args = plot_res_ij)
    plot_res_i <- plot_res_i[, c("timeNumber", "plotId",
                                 paste0(traits[i], "_corr"),
                                 traits[i], paste0(traits[i], "_wt"))]

    plot_res_ref <- left_join(x = plot_res_ref, y = plot_res_i,
                              by = c("timeNumber", "plotId"))

    geno_res_i <- do.call(what = rbind, args = geno_res_ij)
    colnames(geno_res_i)[4:5] <- c(paste0(traits[i], "_pred"), paste0(traits[i], "_sd"))
    geno_res_i <- geno_res_i[, -2]

    # add those results to the reference df
    geno_res_ref <- left_join(x = geno_res_ref, y = geno_res_i,
                              by = c("timeNumber", "genotype"))

    if(!quiet){print(traits[i])}

  } # end loop over traits

  return(list(plot_res = plot_res_ref, geno_res = geno_res_ref,
              comp_monitor = comp_monitor))

}
