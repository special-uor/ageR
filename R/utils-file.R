#' Check file structure
#'
#' Check file structure before running the age model.
#'
#' If only \code{wdir} is passed, the value of \code{entity} will be extracted
#' from the last portion of \code{wdir}. For example, if
#' \code{wdir = "/path/to/a"}, then \code{entity = "a"} and
#' \code{wdir = "/path/to"}. If \code{entity = NULL} and \code{wdir = "/"}
#' the function will return an error message, because the path is too short.
#' To use the current working directory, \code{wdir = "./"}.
#'
#' @param wdir Working directory, absolute or relative path.
#' @param entity Entity name.
#' @param am Age model name.
#'
#' @return Stop the execution if the file structure does not match the expected
#' for the age model.
#'
#' @examples
#' \dontrun{
#'     ageR:::check_files("/", "test")
#' }
#'
#' @noRd
#' @keywords internal
check_files <- function(wdir, entity = NULL, am = "bacon") {
  if (is.null(entity)) {
    entity <- basename(wdir)
    if (entity == "" || entity == ".") {
      stop("The given path (wdir) is too short!")
    }
    wdir <- dirname(wdir)
  }

  # Verify both the working and entity directory exist
  if (!dir.exists(wdir)) {
    stop("The working directory not found")
  } else if (!dir.exists(file.path(wdir, entity))) {
    stop("Entity directory not found")
  }

  if (tolower(am) == "bacon") {
    filenames <- file.path(c(file.path("Bacon_runs",
                                       entity,
                                       c(paste0(entity, "_depths.txt"),
                                         paste0(entity, "_sample_ids.csv"),
                                         paste0(entity, ".csv"))),
                             "hiatus.csv",
                             "not_used_dates.csv"))
    idx <- unlist(lapply(file.path(wdir, entity, filenames), file.exists))
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

#' Create input for age model
#'
#' Create input for age models, based on the file structure and input files
#' oraanisation needed.
#'
#' If only \code{wdir} is passed, the value of \code{entity} will be extracted
#' from the last portion of \code{wdir}. For example, if
#' \code{wdir = "/path/to/a"}, then \code{entity = "a"} and
#' \code{wdir = "/path/to"}. If \code{entity = NULL} and \code{wdir = "/"}
#' the function will return an error message, because the path is too short.
#' To use the current working directory, \code{wdir = "./"}.
#'
#' @param data List with named data frames containing the input data.
#' \itemize{
#'   \item \code{sample}: data frame with samples information.
#'     \itemize{
#'       \item \code{id}: depth ID.
#'       \item \code{depth}: depth in cm.
#'     }
#'   \item \code{core}: data frame with core (entity) information.
#'     \itemize{
#'       \item \code{labID}: lab ID.
#'       \item \code{age}: age in years.
#'       \item \code{error}: age uncertainty in years.
#'       \item \code{depth}: depth in cm.
#'       \item (Optional) \code{cc}: calibration curve.
#'         \itemize{
#'           \item \code{cc = 1}: IntCal20 (northern hemisphere terrestrial).
#'           \item \code{cc = 2}: Marine20 (marine).
#'           \item \code{cc = 3}: SHCal20 (southern hemisphere terrestrial).
#'         }
#'     }
#'  \item (Optional) \code{hiatus}: data frame with information of hiatus
#'  depths, if not passed, then an empty file is created.
#'     \itemize{
#'       \item \code{id}: depth ID.
#'       \item \code{depth}: depth in cm.
#'     }
#' }
#' @param wdir Working directory, absolute or relative path.
#' @param entity Entity name.
#' @param am Age model name.
#'
#' @return
#'
#' @export
#'
#' @examples
#' test_data <-list(sample = data.frame(id = 1:100,
#'                                      depth = seq(0, 500, length = 100)),
#'                  core = data.frame(labID = "RDG",
#'                                    age = c(720, 2700, 4660),
#'                                    error = c(70, 50, 110),
#'                                    depth = c(83, 212, 418)))
#' ageR::create_input(test_data, here::here(), "Reading")
create_input <- function(data, wdir, entity = NULL, am = "bacon") {
  if (is.null(entity)) {
    entity <- basename(wdir)
    if (entity == "" || entity == ".") {
      stop("The given path (wdir) is too short!")
    }
    wdir <- dirname(wdir)
  }

  # Verify both the working and entity directory exist
  if (!dir.exists(wdir)) {
    dir.create(wdir, recursive = TRUE)
  } else if (!dir.exists(file.path(wdir, entity))) {
    dir.create(file.path(wdir, entity), recursive = TRUE)
  }

  if (tolower(am) == "bacon") {
    # Verify input data format
    if (!any(c("sample", "core") %in% names(data))) {
      stop("Wrong format for the data parameter")
    } else if (!any(c("id", "sample") %in% names(data$sample))) {
      stop(paste0("\nThe sample structure must contain 2 variables:",
                  "\n - id",
                  "\n - depth"))
    } else if (!any(c("labID", "age", "error", "depth")
                    %in% names(data$core))) {
      stop(paste0("\nThe core structure must contain 4 variables:",
                  "\n - labID",
                  "\n - age",
                  "\n - error",
                  "\n - depth"))
    }

    # Data for the Bacon model
    dir.create(file.path(wdir, entity, 'Bacon_runs', entity),
               showWarnings = FALSE,
               recursive = TRUE)
    path <- file.path(wdir, entity, 'Bacon_runs', entity)
    write.csv(data$core,
              file.path(path, paste0(entity, ".csv")),
              row.names = FALSE)
    write.table(data$sample$depth,
                file.path(path, paste0(entity, "_depths.txt")),
                row.names = FALSE,
                col.names = FALSE)
    write.csv(data$sample$id,
              file.path(path, paste0(entity, "_sample_ids.csv")),
              row.names = FALSE)

    # Empty data frame for "not used dates"
    setwd(file.path(wdir, entity))
    not_used_dates <- data.frame(depth = NA, error = NA)[-1, ]
    write.csv(not_used_dates, "not_used_dates.csv", row.names = FALSE)

    if ("hiatus" %in% names(data) &&
        !any(c("id", "sample") %in% names(data$haitus))) {
      write.csv(data$hiatus, "hiatus.csv", row.names = FALSE)
    } else {
      # Empty data frame for Hiatus
      hiatus_tb <- data.frame(id = NA, depth = NA)[-1, ]
      write.csv(hiatus_tb, "hiatus.csv", row.names = FALSE)
    }
  } else {
    warning(paste0(am, " is not a valid age model."))
  }
}


#' Show file structure
#'
#' Show expected file structure for each age model.
#'
#' @param entity Entity name.
#' @param am Age model name.
#'
#' @return File (tree) structure.
#' @export
#'
#' @examples
#' ageR::file_structure("Reading")
file_structure <- function(entity, am = "bacon") {
  if (tolower(am) == "bacon") {
    paths <- file.path(entity, c(file.path("Bacon_runs",
                                           entity,
                                           c(paste0(entity, "_depths.txt"),
                                             paste0(entity, "_sample_ids.csv"),
                                             paste0(entity, ".csv"))),
                                 "hiatus.csv",
                                 "not_used_dates.csv"))
    tree <- data.tree::as.Node(data.frame(pathString = paths))
    data.tree::SetGraphStyle(tree, rankdir = "TB")
    data.tree::SetNodeStyle(tree,
                            style = "filled,rounded",
                            shape = "box")
    plot(tree)
    print(tree)
  } else {
    warning(paste0(am, " is not a valid age model."))
  }
}