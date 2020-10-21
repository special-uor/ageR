#' Scenario sequence
#'
#' Create an evenly spaced array from a reference value, \code{ref}, by steps
#' of length \code{step}. To be used to create alternative scenarios.
#'
#' @param ref Reference value.
#' @param step Size of increments.
#' @param lower Lower bound. If not passed, it will default to \code{ref / 2}.
#' @param upper Upper bound. If not passed, it will default to \code{2 * ref}.
#'
#' @return Evenly spaced array.
#'
#' @examples
#' ageR:::sce_seq(50)
#' ageR:::sce_seq(50, lower = 30, upper = 80)
#'
#' @keywords internal
#' @noRd
sce_seq <- function(ref, step = 5, lower = NULL, upper = NULL) {
  if (is.null(lower))
    lower <- floor(ref / 2)
  if (is.null(upper))
    upper <- ceiling(2 * ref)
  return(unique(c(seq(lower, ref, by = step),
                  seq(ref, upper, by = 2 * step))))
}
