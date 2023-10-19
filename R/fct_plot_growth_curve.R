#' plot_growth_curve
#'
#' @description Plot growth curve fundtions for selected genotype.
#'
#' @param raw matrix raw observed data obtained with function growth_curve_fit
#'
#' @param linear matrix linear trend fitted data obtained with function growth_curve_fit
#'
#' @param logistic matrix logistic trend fitted data obtained with function growth_curve_fit
#'
#' @param geno_sel numeric vector selected genotypes
#'
#' @param main plot title
#'
#' @return growth curve plot
#'
#' @export

plot_growth_curve <- function(raw, linear, logistic, geno_sel = NULL, main = "Growth curve plot"){

  if(!is.null(geno_sel)){

    raw <- raw[geno_sel, , drop = FALSE]
    linear <- linear[geno_sel, , drop = FALSE]
    logistic <- logistic[geno_sel, , drop = FALSE]

  }

  n_tp <- ncol(raw)
  n_geno <- nrow(raw)
  geno_id <- rownames(raw)
  tp_id <- colnames(raw)

  df_raw <- data.frame(geno = rep(geno_id, n_tp), tp = rep(tp_id, each = n_geno),
                       y = c(raw), type = "raw")

  df_lin <- df_log <- df_raw

  df_lin$y <- c(linear)
  df_lin$type <- "linear"

  df_log$y <- c(logistic)
  df_log$type <- "logistic"

  d <- rbind(df_raw, df_lin, df_log)

  # transform the time stamp into difference per day
  d$tp <- yday(d$tp)
  d$tp <- d$tp - min(d$tp, na.rm = TRUE) + 1

  d$type <- as.factor(d$type)

  p <- ggplot(data = d, aes(x = tp, y = y, group = type, col = type)) +
    geom_line(linewidth=1.3) + ggtitle(main) +
    theme(title = element_text(size = 20),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 16))

  return(p)

}
