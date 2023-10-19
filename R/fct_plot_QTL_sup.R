#' plot_QTL_sup
#'
#' @description Superimposed plot of QTL profiles
#'
#' @param data QTL profile data obtained with QTL_initial_scan function
#'
#' @param traits character vector of selected traits.
#'
#' @param threshold QTL selection threshold
#'
#' @return The return value, if any, from executing the function.
#'
#' @export


plot_QTL_sup <- function(data, traits, threshold = 3){

  # arrange the data into long format
  tr_res <- data[, 5:ncol(data)]
  # if(is.null(tr_nm)){tr_nm <- colnames(tr_res)} else {tr_nm <- traits}

  tr_res <- tr_res[, traits]

  n_tr <- length(traits)
  pos_vec <- data[, 4]
  chr_vec <- data[, 2]
  n_pos <- length(pos_vec)

  d_long <- data.frame(chr = rep(chr_vec, n_tr),
                       pos = rep(pos_vec, n_tr) + rep(0:(n_tr - 1), each = n_pos),
                       trait = rep(traits, each = n_pos),
                       log10p = c(as.matrix(tr_res)))

  d_long$trait <-  factor(d_long$trait, levels = traits, ordered = TRUE)

  # plot
  p <- ggplot(d_long, aes(x = pos, y = log10p, group = trait, col = trait)) +
    facet_grid(trait~chr) + geom_line() + geom_hline(yintercept = threshold) +
    scale_color_hue()

  return(p)

}
