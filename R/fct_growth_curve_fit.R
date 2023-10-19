#' growth_curve_fit
#'
#' @description fit growth curve to genotype time series data
#'
#' @param data genotype time series data must contain 'genotype', 'timePoint',
#' 'timeNumber', and specified trait columns.
#'
#' @param trait character string indicating the trait.
#'
#' @param type character string indicating the type of growth curve. 'linear' or
#' 'logistic'. Default = 'logistic'
#'
#' @return The return value, if any, from executing the function.
#'
#' @export

growth_curve_fit <- function(data, trait, type = "logistic"){

  # subset and organise the data
  c_nm <- colnames(data)
  sel_col <- c(which(c_nm == "genotype"),
               which(c_nm == "timePoint"),
               which(c_nm == "timeNumber"),
               which(c_nm == trait))


  d <- data[, sel_col]
  colnames(d)[c(1, 3, 4)] <- c("geno", "tp", "trait")
  d$tp <- d$tp - min(d$tp) + 1
  d <- d[order(d$tp), ]
  d$geno <- as.character(d$geno)

  geno_id <- unique(d$geno)
  n_geno <- length(geno_id)
  n_tp <- max(d$tp)
  tp_un <- unique(d$timePoint)

  if(type == "linear"){

    res_param <- matrix(NA, nrow = n_geno, ncol = 3)
    res_raw <- matrix(NA, nrow = n_geno, ncol = n_tp)
    res_fitted_TS <- matrix(NA, nrow = n_geno, ncol = n_tp)

    for(i in 1:n_geno){

      d_i <- d[d$geno == geno_id[i], ]
      m <- tryCatch(lm(trait ~ tp, data = d_i), error = function(x) NULL)

      if(!is.null(m)){

        res_param[i, 1:2] <- m$coefficients
        res_param[i, 3] <- summary(m)$r.squared
        res_fitted_TS[i, which(!is.na(d_i$trait))] <- m$fitted.values
        res_raw[i, d_i$tp] <- d_i$trait

      }

    }

    colnames(res_param) <- c("Int", "slope", "R2")

  } else if (type == "logistic"){

    res_param <- matrix(NA, nrow = n_geno, ncol = 5)
    res_raw <- matrix(NA, nrow = n_geno, ncol = n_tp)
    res_fitted_TS <- matrix(NA, nrow = n_geno, ncol = n_tp)

    for(i in 1:n_geno){

      d_i <- d[d$geno == geno_id[i], ]
      ml <- tryCatch(drm(trait ~ tp, data = d_i, fct = L.4(), type = "continuous"),
                     error = function(x) NULL)

      if(!is.null(ml)){

        res_param[i, 1:4] <- ml$fit$par
        res_param[i, 5] <- tryCatch(cor(ml$predres[, 1], d_i$trait)^2,
                                    error = function(e) NA)
        res_fitted_TS[i, which(!is.na(d_i$trait))] <- ml$predres[, 1]
        res_raw[i, d_i$tp] <- d_i$trait

      }

    }

    colnames(res_param) <- c('b', 'c', 'd', 'e', "R2")

  }

  colnames(res_fitted_TS) <- colnames(res_raw) <- tp_un
  rownames(res_param) <- rownames(res_raw) <- rownames(res_fitted_TS) <- geno_id

  return(list(param = data.frame(res_param), fitted_TS = res_fitted_TS, raw_TS = res_raw))

}
