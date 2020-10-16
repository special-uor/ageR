#' Clean string
#'
#' Clean string by removing accented characters and any other punctuation. To
#' white list (keep) special characters, list them using \code{keep}.
#'
#' @param str Original string.
#' @param rm_wht Boolean, if \code{TRUE} remove white spaces, otherwise
#'     keep them.
#' @param keep String with special characters to keep.
#'
#' @return New string without special characters.
#'
#' @examples
#' ageR:::cln_str("ÀÊ?")
#' ageR:::cln_str("ÀÊ?_AE?")
#' ageR:::cln_str("ÀÊ?.AE?")
#' ageR:::cln_str("ÀÊ?.AE?", keep = "\\._-")
#' ageR:::cln_str("ÀÊ? AE?", rm_wht = TRUE)
#'
#' @noRd
#' @keywords internal
cln_str <- function(str, rm_wht = FALSE, keep = c("_-")) {
  if (is.na(str) || is.null(str))
    return(str)
  new_str <- iconv(str, to = 'ASCII//TRANSLIT') # Remove accented characters
  # Remove punctuation, but keep the characters in the keep parameter
  new_str <- gsub(paste0("(?![", keep, "])[[:punct:]]"),
                  "",
                  new_str,
                  perl = TRUE)
  # Remove white spaces and replace by -
  if (rm_wht)
    new_str <- gsub(" ", "-", new_str)
  return(new_str)
}

#' Display message inside a box
#'
#' @param str String to display.
#' @param quiet Boolean flag to hide messages.
#' @param limit Maximum number of characters, output width.
#'
#' @examples
#' ageR:::msg("A")
#' ageR:::msg("AB")
#' ageR:::msg("ABC")
#' ageR:::msg("ABCD")
#' ageR:::msg("ABCD", limit = 30)
#' ageR:::msg("ABCD", quiet = TRUE)
#'
#' @noRd
#' @keywords internal
msg <- function(str, quiet = FALSE, limit = 80) {
  if (!quiet) {
    len <- nchar(str)
    if (len > limit) {
      message(msg)
    } else {
      pad_r <- floor((limit - len - 2) / 2)
      pad_l <- ceiling((limit - len - 2) / 2)
      message(paste0(rep("-", limit), collapse = ""), "\n|",
              paste0(rep(" ", pad_r - 1), collapse = ""), " ",
              str, " ",
              paste0(rep(" ", pad_l - 1), collapse = ""), "|\n",
              paste0(rep("-", limit), collapse = ""), "\n")
    }
  }
}
