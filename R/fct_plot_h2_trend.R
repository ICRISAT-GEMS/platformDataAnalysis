#' plot_h2_trend
#'
#' @description Plot heritability trend
#'
#' @param h2_LC heritability results for loadcell (transpiration) data obtained
#' from function spatial_adjustment.
#'
#' @param h2_PE heritability results for loadcell (transpiration) data obtained
#' from function spatial_adjustment.
#'
#' @param tp_conv specify the way tp are transformed in x axis. Either, follow
#' the number of days in the date "day_gap" or tp is converted into 1, 2, ...
#' "consecutive"
#'
#' @param main graph title
#'
#' @return plot of the heritabilities per trait over time.
#'
#' @export

plot_h2_trend <- function(h2_LC, h2_PE, tp_conv = "day_gap",
                          main = "heritability trend"){

  # LC data
  n_tp <- ncol(h2_LC)
  tp_id <- colnames(h2_LC)
  n_trait <- nrow(h2_LC)
  trait_id <- rownames(h2_LC)

  df_LC <- data.frame(trait = rep(trait_id, n_tp), tp = rep(tp_id, each = n_trait),
                      h2 = c(h2_LC))

  # PE data
  n_tp <- ncol(h2_PE)
  tp_id <- colnames(h2_PE)
  n_trait <- nrow(h2_PE)
  trait_id <- rownames(h2_PE)

  df_PE <- data.frame(trait = rep(trait_id, n_tp), tp = rep(tp_id, each = n_trait),
                      h2 = c(h2_PE))

  d <- rbind(df_LC, df_PE)

  if(tp_conv == "day_gap"){

    # transform the time stamp into difference per day
    d$tp <- yday(d$tp)
    d$tp <- d$tp - min(d$tp, na.rm = TRUE) + 1

  } else if (tp_conv == "consecutive") {

    d <- add_timeNumber(data = d, tp_col_nm = "tp")
    d$tp <- d$timeNumber

  }


  d$trait <- as.factor(d$trait)

  p <- ggplot(data = d, aes(x = tp, y = h2, group = trait, col = trait)) +
    geom_line(linewidth=1) + ggtitle(main)

  return(p)

}
