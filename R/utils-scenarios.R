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
#' \dontrun{
#' ageR:::sce_seq(50, lower = 30, upper = 40)
#' }
#'
#' @keywords internal
#' @noRd
sce_seq <- function(ref, step = 5, lower = NULL, upper = NULL) {
  if (!is.null(upper) && (upper < ref))
    stop("\nThe upper bound cannot be smaller than ", ref, ".")
  if (!is.null(lower) && (lower > ref))
    stop("\nThe lower bound cannot be larger than ", ref, ".")
  if (is.null(lower))
    lower <- floor(ref / 2)
  if (is.null(upper))
    upper <- ceiling(2 * ref)
  return(unique(c(seq(lower, ref, by = step),
                  seq(ref, upper, by = 2 * step))))
}

#' @keywords internal
done <- function(wdir, entity) {
  tryCatch(
  write(paste0("Bacon execution over. \nDone: ", Sys.time()),
       file = paste0(file.path(wdir, entity), ".ageR"),
       append = TRUE),
  error = function(e) {
    warning("Something went wrong:\n", e)
  })
}

#' @keywords internal
is.done <- function(wdir, entity) {
  if (file.exists(paste0(file.path(wdir, entity), ".ageR")))
    return(TRUE)
  return(FALSE)
}

#' Show progress bar
#'
#' @param expr R expression.
#' @inheritDotParams progressr::with_progress -handlers
#'
#' @return Return data from the function called.
#' @export
pb <- function(expr, ...) {
  progress_bar <-
    progressr::handler_progress(format = "(:current/:total) [:bar] :percent",
                                width = 80)
  progressr::with_progress(expr,
                           ...,
                           handlers = progress_bar)
}
