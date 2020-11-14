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
#' organisation needed.
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
#'   \item \code{sample_depths}: data frame with sampling depths information.
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
#'           \item \code{cc = 1}: \code{IntCal20}
#'           (northern hemisphere terrestrial).
#'           \item \code{cc = 2}: \code{Marine20} (marine).
#'           \item \code{cc = 3}: \code{SHCal20}
#'           (southern hemisphere terrestrial).
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
#' @return Input files for the age model.
#'
#' @export
#'
#' @examples
#' test_data <-list(sample_depths = data.frame(id = 1:100,
#'                                             depth =
#'                                               seq(0, 500, length = 100)),
#'                  core = data.frame(labID = "RDG",
#'                                    age = c(720, 2700, 4660),
#'                                    error = c(70, 50, 110),
#'                                    depth = c(83, 212, 418)))
#' \dontrun{
#' ageR::create_input(test_data, getwd(), "Reading")
#' }
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
    if (!any(c("sample_depths", "core") %in% names(data))) {
      stop("Wrong format for the data parameter")
    } else if (!any(c("id", "sample") %in% names(data$sample))) {
      stop(paste0("\nThe sampling depths (sample_depths) structure ",
                  "must contain 2 variables:",
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

    if ("not_used" %in% names(data) && !is.null(data$not_used)) {
      print(data$not_used)
      path <- file.path(wdir, entity)
      write.csv(data$not_used,
                file.path(path, "not_used_dates.csv"),
                row.names = FALSE)
    } else {
      # Empty data frame for "not used dates"
      path <- file.path(wdir, entity)
      not_used_dates <- data.frame(labID = NA,
                                   age = NA,
                                   error = NA,
                                   depth = NA)[-1, ]
      write.csv(not_used_dates,
                file.path(path, "not_used_dates.csv"),
                row.names = FALSE)
    }

    if ("hiatus" %in% names(data) &&
        all(c("id", "depth") %in% names(data$hiatus))) {
      write.csv(data$hiatus,
                file.path(path, "hiatus.csv"),
                row.names = FALSE)
    } else {
      # Empty data frame for Hiatus
      hiatus_tb <- data.frame(id = NA, depth = NA)[-1, ]
      write.csv(hiatus_tb,
                file.path(path, "hiatus.csv"),
                row.names = FALSE)
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

#' Create symbolic link
#'
#' Create symbolic link \code{to} file using the function
#' \code{\link{file.symlink}}.
#'
#' @param from Source file.
#' @param to Target file (link).
#' @param overwrite Boolean flag that deletes existing file or link.
#'
#' @noRd
#' @keywords internal
sym_link <- function(from, to, overwrite = TRUE) {
  if (file.exists(to) && overwrite)
    file.remove(to)
  . <- file.symlink(from = from, to = to)
}

#' Find adjusted K
#'
#' Find adjusted number of sections in the core.
#'
#' @param K Number of sections in the core.
#' @inheritParams run_bacon
#'
#' @return Adjusted \code{K}.
#' @keywords internal
find_K <- function(K, wdir, entity) {
  Ks <- K + c(-10:10)
  paths <- paste0(file.path(wdir, entity), "_", Ks, ".out")
  idx <- unlist(lapply(paths, file.exists))

  # Try to find K from existing files with .bacon extension
  if (sum(idx) == 0) {
    paths <- list.files(path = wdir, pattern = ".bacon$")
    Ks <- unlist(lapply(paths,
                        gsub,
                        pattern = paste0("^", entity, "_"),
                        replace = ""))
    Ks <- unlist(lapply(Ks,
                        gsub,
                        pattern = paste0(".bacon$"),
                        replace = ""))
    tryCatch(Ks <- as.numeric(Ks),
             error = function(e) {
               stop("\nNo Bacon output files were found inside: \n", wdir)
             })
    paths <- file.path(wdir, paths)
    idx <- unlist(lapply(paths, file.exists))
  }
  if (sum(idx) == 0) {
    stop("\nNo Bacon output files were found inside: \n", wdir)
  } else if (sum(idx) > 1) {
    times_idx <- order(unlist(lapply(paths[idx], file.mtime)),
                       decreasing = TRUE)
    warning("\nBacon output files for multiple executions were found, ",
            "using the newest: \n",
            basename(paths[idx][times_idx][1]))

    return(Ks[idx][times_idx][1])
  }
  return(Ks[idx])
}

#' Get absolute path
#'
#' @param path String with path to file or directory.
#'
#' @return String with the absolute path.
#'
#' @keywords internal
absolute_path <- function(path) {
  if (!R.utils::isAbsolutePath(path))
    path <- R.utils::getAbsolutePath(path)
  return(path)
}
