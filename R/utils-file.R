#' Check file structure
#'
#' Check file structure before running the age model.
#'
#' If only \code{wdir} is passed, the value of \code{entity} will be extracted
#' from the last portion of \code{wdir}. For example, if
#' \code{wdir = "/path/to/a"}, then \code{entity = "a"} and
#' \code{wdir = "/path/to"}. If \code{entity = NULL} and \code{wdir = "/"}
#' the function will return an error message, because the path is too short.
#'
#' @param wdir Working directory, absolute or relative path.
#' @param entity Entity name.
#'
#' @return Stop the execution if the file structure does not match the expected
#' for the age model.
#'
# @examples
#'
#' @noRd
#' @keywords internal
check_files <- function(wdir, entity = NULL, am = "bacon") {
  if (is.null(entity)) {
    entity <- basename(wdir)
    if (entity == "" || entity == ".") {
      stop("The given path (wdir) is too short!")
    }
    wdir <- gsub(paste0(entity, "\\/$"), "", wdir)
  }
  # Verify both the working and entity directory exist
  if (!dir.exists(wdir)) {
    stop("The working directory not found")
  } else if (!dir.exists(file.path(wdir, entity))) {
    stop("Entity directory not found")
  }

  # Change path to entity directory
  setwd(file.path(wdir, entity))
  if (tolower(am) == "bacon") {
    filenames <- file.path(c(file.path("Bacon_runs/",
                                       entity,
                                       c(paste0(entity, "_depths.txt"),
                                         paste0(entity, "_sample_ids.csv"),
                                         paste0(entity, ".csv"))),
                             "hiatus.csv",
                             "not_used_dates.csv"))
    idx <- unlist(lapply(filenames, file.exists))
    if (!all(idx)) {
      stop(paste0("\nThe following input file",
                  ifelse(sum(!idx) > 1, "s were", " was"),
                  " not found inside the entity directory [",
                  file.path(entity),
                  "]\n",
                  paste0("- ", filenames[!idx], collapse = "\n")))
    }
  } else {
    warning(paste0(am, " is not a valid age model."))
  }
}
