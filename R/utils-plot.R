plot_log_posterior_base <- function(posterior) {
  plot(x = seq_len(length(posterior)) - 1,
       y = -posterior,
       type = "l",
       ylab = "Log of Objective",
       xlab = "Iteration",
       main = paste0("Variance: ", round(var(posterior), digits = 4)),
       col = grey(0.4))
  abline(h = -mean(posterior), col = "red")
}

#' Plot log of posterior
#'
#' @param posterior Posterior data.
#'
#' @return \code{ggplot2} object.
#'
#' @keywords internal
#' @noRd
plot_log_posterior <- function(posterior) {
  df <- data.frame(x = seq_len(length(posterior)) - 1,
                   y = -posterior)
  p <- ggplot2::ggplot(data = df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line(alpha = 0.7) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    ggplot2::labs(x = "Log of Objective",
                  y = "Iteration",
                  title = paste0("Variance: ",
                                 round(var(posterior), digits = 4))) +
    ggplot2::geom_hline(yintercept = -mean(posterior), col = "red", lty = 2) +
    ggplot2::theme_bw()
  return(p)
}

#' Plot Age-Depth
#'
#' @param df Data frame with age-depth and 95% CI interval.
#' @param hiatuses Data frame with hiatuses depths.
#'
#' @return \code{ggplot2} object
#'
#' @keywords internal
#' @noRd
plot_age_depth <- function(df, hiatuses = NULL) {
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::geom_line(ggplot2::aes(x, y), col = "black") +
    ggplot2::geom_line(ggplot2::aes(x, q5), col = "red", lty = 2) +
    ggplot2::geom_line(ggplot2::aes(x, q95), col = "red", lty = 2) +
    ggplot2::geom_point(ggplot2::aes(depth * 10, age),
                        data = core,
                        fill = core$col,
                        size = 2,
                        shape = 24) +
    # ggplot2::scale_colour_manual(values = core$col) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = nrow(core))) +
    ggplot2::labs(x = "Depth from top [mm]",
                  y = "cal Age [yrs BP]",
                  title = entity) +
    # ggplot2::coord_cartesian(xlim = c(0, max(depths_eval * 10))) +
    # ggplot2::coord_cartesian(xlim = c(0, max(depths_eval * 10)),
    #                          ylim = c(0, max(df$q95))) +
    # ggplot2::scale_x_continuous(limits = c(0, max(depths_eval * 10))) +
    # ggplot2::scale_y_continuous(limits = c(0, max(df$q95))) +
    ggplot2::theme_bw()
  if (!is.null(hiatuses))
    for (i in seq_len(nrow(hiatuses))) {
      p <- p +
        ggplot2::geom_vline(xintercept = hiatuses[i, 2] * 10,
                            col = "grey",
                            lty = 2)
    }
  return(p)
}

plot_acc_prior <- function(prior) {

}
