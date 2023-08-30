#' spatial_adjustment_ij
#'
#' @description function to calculate the phenotypic adjusted values
#' for a specific combination of trait (i) and timepoint (j). The function
#' try different solution from the most complex to the most simple:
#' a) (if fixed effect specified) fixed effect + spatial correction; b)
#' (if previous fail) spatial correction only; c) (if everything fail)
#' simple mean over genotype or row values for plot level. The simple average
#' is not performed if the genotype term is specified as random because the
#' difference of range between term averaged and term adjusted from a random term
#' can be too large.
#'
#' @return list with phenotype and genotype corrected values
#'
#' @noRd

spatial_adjustment_ij <- function(TP, tr_id, tp, extraFixedFactors, geno.decomp,
                                  what, useCheck, useRepId, engine, spatial,
                                  quiet) {

  if(!is.null(extraFixedFactors)){

    # opt 1: fixed terms + surface
    tr_adj <- tryCatch(fitModels(TP = TP, trait = tr_id, timePoints = tp,
                                 extraFixedFactors = extraFixedFactors,
                                 geno.decomp = geno.decomp, what = what,
                                 useCheck = useCheck, useRepId = useRepId,
                                 engine = engine, spatial = spatial,
                                 quiet = quiet), error = function(e) NULL)

    if(is.null(tr_adj)){

      # opt 2: surface only
      tr_adj <- tryCatch(fitModels(TP = TP, trait = tr_id, timePoints = tp,
                                   extraFixedFactors = NULL,
                                   geno.decomp = geno.decomp, what = what,
                                   useCheck = useCheck, useRepId = useRepId,
                                   engine = engine, spatial = spatial,
                                   quiet = quiet), error = function(e) NULL)

      if(is.null(tr_adj)){

        # opt 3: no correction
        d <- TP[[tp]]

        # plot adjustment
        plot_adj <- data.frame(timeNumber = tp, timePoint = d$timePoint,
                               trait_corr = d[, tr_id], trait = d[, tr_id],
                               wt = NA, genotype = d$genotype,
                               d[, extraFixedFactors], rowId = d$rowId,
                               colId = d$colId, plotId = d$plotId)

        colnames(plot_adj)[3:5] <- c(paste0(tr_id, "_corr"), tr_id,
                                     paste0(tr_id, "_wt"))
        colnames(plot_adj)[grep(pattern = "extraFixedFactors",
                                x = colnames(plot_adj))] <- extraFixedFactors

        # geno adjustment
        d_red <- d[, c("genotype", tr_id)]
        colnames(d_red)[2] <- "trait"
        mean_mdf <- function(x) mean(x = x, na.rm = TRUE)
        d_red <- d_red %>% summarise(m = mean_mdf(trait), .by = genotype)

        geno_adj <- data.frame(timeNumber = tp, timePoint = d$timePoint[1],
                               genotype = d_red$genotype,
                               predicted.values = d_red$m,
                               standard.errors = NA)

        if(what == "random"){

          plot_adj[, 3] <- NA
          geno_adj$predicted.values <- NA
          adj_type = "failed"

        } else {

          adj_type = "no_correction"

        }

        return(list(plot_adj = plot_adj, geno_adj = geno_adj, adj_type = adj_type))

      } else {

        plot_adj <- getCorrected(tr_adj)

        # add the fixed effects column
        d_fix <- TP[[tp]][, extraFixedFactors, drop = FALSE]
        geno_pos <- which(colnames(plot_adj) == "genotype")
        plot_adj <- data.frame(plot_adj[, 1:geno_pos], d_fix,
                               plot_adj[, (geno_pos + 1):ncol(plot_adj)])
        colnames(plot_adj)[5] <- paste0(tr_id, "_wt")

        geno_adj <- getGenoPred(tr_adj)
        geno_adj <- geno_adj$genoPred
        return(list(plot_adj = plot_adj, geno_adj = geno_adj,
                    adj_type = "spatial_only"))

      }

    } else{

      plot_adj <- getCorrected(tr_adj)
      colnames(plot_adj)[5] <- paste0(tr_id, "_wt")
      geno_adj <- getGenoPred(tr_adj)
      geno_adj <- geno_adj$genoPred
      return(list(plot_adj = plot_adj, geno_adj = geno_adj,
                  adj_type = "design_and_spatial"))

    }


  } else {

    # opt 2: surface only
    tr_adj <- tryCatch(fitModels(TP = TP, trait = tr_id, timePoints = tp,
                                 extraFixedFactors = NULL,
                                 geno.decomp = geno.decomp, what = what,
                                 useCheck = useCheck, useRepId = useRepId,
                                 engine = engine, spatial = spatial,
                                 quiet = quiet), error = function(e) NULL)

    if(is.null(tr_adj)){

      # opt 3: no correction
      d <- TP[[tp]]

      # plot adjustment
      plot_adj <- data.frame(timeNumber = tp, timePoint = d$timePoint,
                             trait_corr = d[, tr_id], trait = d[, tr_id],
                             wt = NA, genotype = d$genotype,
                             rowId = d$rowId, colId = d$colId,
                             plotId = d$plotId)

      colnames(plot_adj)[3:5] <- c(paste0(tr_id, "_corr"), tr_id,
                                   paste0(tr_id, "_wt"))


      # geno adjustment
      d_red <- d[, c("genotype", tr_id)]
      colnames(d_red)[2] <- "trait"
      mean_mdf <- function(x) mean(x = x, na.rm = TRUE)
      d_red <- d_red %>% summarise(m = mean_mdf(trait), .by = genotype)

      geno_adj <- data.frame(timeNumber = tp, timePoint = d$timePoint[1],
                             genotype = d_red$genotype,
                             predicted.values = d_red$m,
                             standard.errors = NA)



      if(what == "random"){

        plot_adj[, 3] <- NA
        geno_adj$predicted.values <- NA
        adj_type = "failed"

      } else {

        adj_type = "no_correction"

      }

      return(list(plot_adj = plot_adj, geno_adj = geno_adj, adj_type = adj_type))

    } else {

      plot_adj <- getCorrected(tr_adj)
      colnames(plot_adj)[5] <- paste0(tr_id, "_wt")
      geno_adj <- getGenoPred(tr_adj)
      geno_adj <- geno_adj$genoPred
      return(list(plot_adj = plot_adj, geno_adj = geno_adj, adj_type = "spatial_only"))

    }

  }

}
