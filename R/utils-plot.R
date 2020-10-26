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
    ggplot2::geom_hline(yintercept = -mean(posterior), colour = "red") +
    ggplot2::theme_bw()
  return(p)
}

plot_acc_prior <- function(prior) {

}
