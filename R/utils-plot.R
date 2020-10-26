#' Plot log of posterior
#'
#' @param posterior Posterior data.
#'
#' @return \code{ggplot2} object.
#'
#' @keywords internal
#' @noRd
plot_log_post <- function(posterior) {
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
#' @param core Data frame with the core data.
#' @param entity Entity name.
#' @param hiatuses Data frame with hiatuses depths.
#'
#' @return \code{ggplot2} object
#'
#' @keywords internal
#' @noRd
plot_age_depth <- function(df, core, entity = NULL, hiatuses = NULL) {
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

#' Plot prior accumulation rate
#'
#' @param acc.mean Accumulation rate mean.
#' @param acc.shape Accumulation rate shape.
#' @param xlim X-axis limits.
#'
#' @return \code{ggplot2} object.
#'
#' @keywords internal
#' @noRd
plot_acc_prior <- function(acc.mean = 20,
                           acc.shape = 1.5,
                           xlim = c(0, 3 * max(acc.mean))) {
  p <- ggplot2::ggplot(data = data.frame(x = c(0, max(xlim))),
                       ggplot2::aes(x)) +
    ggplot2::stat_function(fun =
                             function(x) {
                               dgamma(x,
                                      shape = acc.shape,
                                      rate = acc.shape / acc.mean)},
                           col = "green",
                           lwd = 2) +
    ggplot2::labs(x = "Acc. rate [yr/cm]",
                  y = NULL) +
    ggplot2::theme_bw()
  return(p)
}
