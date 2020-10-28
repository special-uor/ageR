#' Plot log of posterior
#'
#' @importFrom stats var
#'
#' @param data Posterior data.
#' @param varp Variance percentage threshold.
#'
#' @return List with \code{ggplot2} object and variance.
#'
#' @keywords internal
plot_log_post <- function(data, varp = NULL) {
  # Local binding
  x <- y <- NULL
  if (!is.null(varp))
    data <- data[data > mean(data) * (1 - varp) &
                 data < mean(data) * (1 + varp)]
  df <- data.frame(x = seq_len(length(data)) - 1,
                   y = -data)
  p <- ggplot2::ggplot(data = df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line(alpha = 0.7) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    ggplot2::labs(x = "Log of Objective",
                  y = "Iteration",
                  title = paste0("Variance: ",
                                 round(var(data), digits = 4))) +
    ggplot2::geom_hline(yintercept = -mean(data), col = "red", lty = 2) +
    ggplot2::theme_bw()
  return(list(plot = p, var = var(data)))
}

#' Plot Age-Depth
#'
#' @param df Data frame with age-depth and 95\% CI interval.
#' @param core Data frame with the core data.
#' @param entity Entity name.
#' @param hiatuses Data frame with hiatuses depths.
#'
#' @return \code{ggplot2} object.
#'
#' @keywords internal
plot_age_depth <- function(df, core, entity = NULL, hiatuses = NULL) {
  # Local binding
  x <- y <- q5 <- q95 <- depth <- age <- NULL
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
#' @importFrom stats density
#' @importFrom stats dgamma
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
  # Local binding
  x <- NULL
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
#' @importFrom stats density
#' @importFrom stats dgamma
#' @param K Number of sections in the core.
#' @param output Last MCMC output.
#' @param hiatuses Data frame with hiatus depths.
#' @param standalone Boolean flag to indicate whether or not the plot is a
#'     layer for another plot or standalone.
#' @inheritParams runBacon
#'
#' @return List with \code{ggplot2} object and data frame with posterior and
#'     prior values.
#'
#' @keywords internal
plot_acc_post <- function(K,
                          output,
                          acc.mean = 20,
                          acc.shape = 1.5,
                          thick = 5,
                          hiatuses = NULL,
                          standalone = TRUE) {
  # Local binding
  x <- y <- NULL
  depths <- seq(0, thick * (K - 1), thick)
  idx <- 2:(K - 1)
  if (!is.null(hiatuses) & nrow(hiatuses) > 0) {
    for (h in hiatuses[, 2])
      idx <- idx[-max(which(depths < h))]
  }
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

#' Plot accumulation rate
#'
#' Plot accumulation rate prior and posterior.
#'
#' @param K Number of sections in the core.
#' @param output Last MCMC output.
#' @param acc.mean Accumulation rate mean.
#' @param acc.shape Accumulation rate shape.
#' @param hiatuses Data frame with hiatus depths.
#' @param plot Boolean flag to indicate whether a plot should be generated or
#'     just return a data frame with posterior and prior values.
#' @param xlab \code{x}-axis label.
#' @param ylab \code{y}-axis label.
#' @param title Plot title.
#' @param ... Optional parameters for
#'     \code{\link[ggplot2:stat_function]{ggplot2::stat_function}}.
#'
#' @return List with \code{ggplot2} object and data frame with posterior and
#'     prior values.
#'
#' @examples
#' \dontrun{
#' out <- read.table("Bacon_runs/core/core_K.out")
#' plot_acc(out$K, out$output)
#' }
#'
#' @keywords internal
plot_acc <- function(K,
                     output,
                     acc.mean = 20,
                     acc.shape = 1.5,
                     hiatuses = NULL,
                     plot = TRUE,
                     xlab = "Acc. rate [yr/cm]",
                     ylab = NULL,
                     title = NULL,
                     ...) {
  out <- plot_acc_post(K, output, acc.mean, acc.shape, hiatuses, FALSE)
  p <- out$plot +
    plot_acc_prior(acc.mean, acc.shape, standalone = FALSE, ...) +
    ggplot2::labs(x = xlab,
                  y = ylab,
                  title = title) +
    ggplot2::theme_bw()
  if (plot)
    print(p)
  return(list(plot = p, data = out$data))
}

#' Plot area between curves
#'
#' Plot area between curves, posterior and prior.
#'
#' @param data Data frame with posterior and prior values. This can be obtained
#'     with \code{\link{plot_acc}}.
#' @param fill Filling colour between curves.
#' @param alpha Transparency value for the shaded area between curves.
#' @param plot Boolean flag to indicate whether a plot should be generated or
#'     just return a data frame with posterior and prior values.
#' @param xlab \code{x}-axis label.
#' @param ylab \code{y}-axis label.
#' @param title Plot title.
#' @param ... Optional parameters for
#'     \code{\link[ggplot2:stat_function]{ggplot2::geom_ribbon}}.
#'
#' @return List with \code{ggplot2} object and area between curves (ABC).
#'
#' @examples
#' \dontrun{
#' out <- read.table("Bacon_runs/core/core_K.out")
#' out <- plot_acc(out$K, out$output)
#' plot_abc(out$data)
#' }
#'
#' @keywords internal
plot_abc <- function(data,
                     fill = "#2980B9",
                     alpha = 0.7,
                     plot = TRUE,
                     xlab = NULL,
                     ylab = NULL,
                     title = NULL,
                     ...) {
  # Local binding
  x <- post <- prior <- NULL
  abc <- sum(with(data, abs(prior - post)))
  title <- paste0(title,
                  ifelse(is.null(title), "", " - "),
                  "Area between curves: ",
                  round(abc, digits = 4))
  p <- ggplot2::ggplot(data, ggplot2::aes(x, post)) +
    ggplot2::geom_line(ggplot2::aes(y = post), lwd = 1) +
    ggplot2::geom_line(ggplot2::aes(y = prior), lwd = 1) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = prior, ymax = post),
                         fill = fill,
                         alpha = alpha,
                         ...) +
    ggplot2::labs(x = xlab,
                  y = ylab,
                  title = title) +
    ggplot2::theme_bw()
  if (plot)
    print(p)
  return(list(plot = p, abc = abc))
}

#' Create grid of plots
#'
#' Create grid of \code{ggplot2} objects, based on a data frame with scenarios.
#'
#' @param plots List with \code{ggplot2} objects.
#' @param scenarios Data frame with scenarios, must contain only 2 variables.
#' @param cond_x Condition on the \code{x}-axis.
#' @param cond_y Condition on the \code{y}-axis.
#' @param cond_x_units Units for condition on the \code{x}-axis.
#' @param cond_y_units Units for condition on the \code{y}-axis.
#' @param ... Optional parameters for
#'     \code{\link[gridExtra:grid.arrange]{gridExtra::grid.arrange}}.
#'
#' @return List of gridded \code{ggplot2} objects.
#'
#' @keywords internal
plot_grid <- function(plots,
                      scenarios,
                      cond_x = "x",
                      cond_y = "y",
                      cond_x_units = NULL,
                      cond_y_units = NULL,
                      append_title = FALSE,
                      ...) {
  # Extract unique labels that make each scenario combination
  labels_cond_x <- unique(scenarios[, 1])
  labels_cond_y <- unique(scenarios[, 2])
  # Remove labels
  for (i in seq_len(length(plots))) {
    tmp <- plots[[i]]
    idx <- arrayInd(i, c(length(labels_cond_x), length(labels_cond_y))) # length(accMean), length(thickness)))
    idx_x <- idx[, 1]
    idx_y <- idx[, 2]
    tmp$labels$x <- NULL
    if (idx_x == 1) {
      tmp$labels$y <- paste0(cond_y,
                             ifelse(is.null(cond_y), "", ": "),
                             labels_cond_y[idx_y],
                             cond_y_units)
    } else {
      tmp$labels$y <- NULL
    }
    if (append_title) {
      tmp$labels$title <- paste0(tmp$labels$title, " | ",
                                 cond_x,
                                 ifelse(is.null(cond_x), "", ": "),
                                 labels_cond_x[idx_x],
                                 cond_x_units)
    } else {
      tmp$labels$title <- paste0(cond_x,
                                 ifelse(is.null(cond_x), "", ": "),
                                 labels_cond_x[idx_x],
                                 cond_x_units)
    }
    # Remove any additional labels
    plot_labels <- names(tmp$labels)
    plot_labels <- plot_labels[!(plot_labels %in% c("x", "y", "title"))]
    tmp$labels[plot_labels] <- NULL

    plots[[i]] <- tmp
  }

  # return(cowplot::plot_grid(plotlist = plots, nrow = length(labels_cond_y)))
  return(gridExtra::grid.arrange(grobs = plots,
                          nrow = length(labels_cond_y),
                          ...))
}
