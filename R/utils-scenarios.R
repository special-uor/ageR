#' Scenario sequence
#'
#' Create an evenly spaced array from a reference value, \code{ref}, by steps
#' of length \code{step}. To be used to create alternative scenarios.
#'
#' @param ref Reference value.
#' @param step Size of increments.
#'
#' @return Evenly spaced array.
#'
#' @examples
#' sce_seq(50)
#'
#' @keywords internal
#' @noRd
sce_seq <- function(ref, step = 5) {
  return(unique(c(seq(floor(ref / 2), ref, by = step),
                  seq(ref, ceiling(2 * ref), by = 2 * step))))
}
