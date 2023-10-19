#' QTL_initial_scan
#'
#' @description Perform an initial QTL scan on a list of traits.
#'
#' @param mppData mppData object containing phenotypic and genotypic data.
#'
#' @param traits character vector list of traits.
#'
#' @param Q.eff type of QTL effect
#'
#' @param threshold numeric threshold value for QTL selection
#'
#' @param window numeric value for exclusion window in the QTL selection procedure.
#'
#' @param n.cores number of cores.
#'
#' @param verbose logical value indicating if traits indicator should be printed
#'
#' @return The return value, if any, from executing the function.
#'
#' @export

QTL_initial_scan <- function(mppData, traits, Q.eff = "par", threshold = 3,
                             window = 50, n.cores = 1, verbose = TRUE){

  n_trait <- length(traits)
  Qprof <- mppData$map
  N_QTL <- rep(NA, n_trait)
  R2 <- rep(NA, n_trait)

  for(i in 1:n_trait){

    # QTL scan
    SIM <- tryCatch(mpp_SIM(mppData = mppData, trait = traits[i], Q.eff = Q.eff,
                            n.cores = n.cores), error = function(x) NULL)

    if(!is.null(SIM)){

      print(plot(SIM, threshold = 3, main = traits[i]))

      Qprof <- data.frame(Qprof, SIM$log10pval)
      colnames(Qprof)[ncol(Qprof)] <- traits[i]

      # N QTL
      QTL <- QTL_select(Qprof = SIM, window = window, threshold = threshold)
      if(!is.null(QTL)){

        N_QTL[i] <- nrow(QTL)
        # R2
        R2_i <- QTL_R2(mppData = mppData, trait = traits[i],
                       QTL = QTL, Q.eff = "par")

        R2[i] <- R2_i$glb.adj.R2

      } else {

        N_QTL[i] <- 0
        R2[i] <- 0

      }

    }

    if(verbose){ print(traits[i])}

  }

  names(N_QTL) <- names(R2) <- traits

  return(list(Qprof = Qprof, N_QTL = N_QTL, R2 = R2))

}
