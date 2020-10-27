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
#' @param standalone Boolean flag to indicate whether or not the plot is a
#'     layer for another plot or standalone.
#' @param xlab \code{x}-axis label.
#' @param ylab \code{y}-axis label.
#' @param title Plot title.
#' @param col Prior curve colour.
#' @param lwd Curve thickness.
#' @param ... Optional parameters for
#'     \code{\link[ggplot2:stat_function]{ggplot2::stat_function}}.
#' @return \code{ggplot2} object.
#'
#' @keywords internal
#' @noRd
plot_acc_prior <- function(acc.mean = 20,
                           acc.shape = 1.5,
                           xlim = c(0, 3 * max(acc.mean)),
                           standalone = TRUE,
                           xlab = "Acc. rate [yr/cm]",
                           ylab = NULL,
                           title = NULL,
                           col = "#00CC00",
                           lwd = 1.5,
                           ...) {
  if (!standalone) {
    p <- ggplot2::stat_function(fun =
                                  function(x) {
                                    dgamma(x,
                                           shape = acc.shape,
                                           rate = acc.shape / acc.mean)},
                                col = col,
                                lwd = lwd,
                                ...)
  } else {
    p <- ggplot2::ggplot(data = data.frame(x = c(0, max(xlim))),
                         ggplot2::aes(x)) +
      ggplot2::stat_function(fun =
                               function(x) {
                                 dgamma(x,
                                        shape = acc.shape,
                                        rate = acc.shape / acc.mean)},
                             col = col,
                             lwd = lwd,
                             ...) +
      ggplot2::labs(x = xlab,
                    y = ylab,
                    title = title) +
      ggplot2::theme_bw()
  }
  return(p)
}


#' Plot accumulation rate posterior
#'
#' @param K Number of sections in the core.
#' @param output Last MCMC output.
#' @param acc.mean Accumulation rate mean.
#' @param acc.shape Accumulation rate shape.
#' @param hiatuses Data frame with hiatus depths.
#' @param standalone Boolean flag to indicate whether or not the plot is a
#'     layer for another plot or standalone.
#'
#' @return \code{ggplot2} object.
#'
#' @keywords internal
#' @noRd
plot_acc_post <- function(K,
                          output,
                          acc.mean = 20,
                          acc.shape = 1.5,
                          hiatuses = NULL,
                          standalone = TRUE) {
  idx <- 2:(K - 1)
  post <- c()
  for (i in idx)
    post <- c(post, output[[i]])
  post <- density(post, from = 0)
  post <- cbind(c(0, post$x, max(post$x)), c(0, post$y, 0))
  maxprior <- dgamma(x = (acc.shape - 1) / (acc.shape / acc.mean),
                     shape = acc.shape,
                     rate = acc.shape / acc.mean)
  if (is.infinite(max(maxprior))) {
    max.y <- max(post[, 2])
  } else {
    max.y <- max(maxprior, post[, 2])
  }

  df <- data.frame(x = post[, 1], y = post[, 2])
  df$prior <- dgamma(x = df$x,
                     shape = acc.shape,
                     rate = acc.shape / acc.mean)
  if (standalone) {
    p <- ggplot2::ggplot(data = df, ggplot2::aes(x, y)) +
      ggplot2::geom_area(alpha = 0.7) +
      ggplot2::geom_line() +
      # plot_acc_prior(acc.mean, acc.shape, standalone = FALSE) +
      ggplot2::labs(x = "Acc. rate [yr/cm]",
                    y = NULL) +
      ggplot2::theme_bw()
    # print(p)
    # colnames(df)[2] <- "observed"
    # return(df)
  } else {
    p <- ggplot2::ggplot(data = df, ggplot2::aes(x, y)) +
      ggplot2::geom_area(alpha = 0.7) +
      ggplot2::geom_line()
  }
  colnames(df)[2] <- "post"
  return(list(plot = p, data = df))
}
