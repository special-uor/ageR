#' @keywords internal
"_PACKAGE"

#' Bacon age model
#'
#' @importFrom foreach %dopar%
#' @importFrom utils capture.output
# @importFrom utils setTxtProgressBar txtProgressBar
#'
#' @param wdir Path where input files are stored.
#' @param entity Name of the entity.
#' @param cpus Number of CPUs to be used on the computation of the age models.
#' @param postbomb Use a postbomb curve for negative (i.e. postbomb) 14C ages.
#'     \code{0 = none}, \code{1 = NH1}, \code{2 = NH2}, \code{3 = NH3},
#'     \code{4 = SH1-2}, \code{5 = SH3}.
#' @param cc Calibration curve for C-14 dates:
#'     \code{cc = 1} for \code{IntCal20} (northern hemisphere terrestrial),
#'     \code{cc = 2} for \code{Marine20} (marine),
#'     \code{cc = 3} for \code{SHCal20} (southern hemisphere terrestrial).
#'     For dates that are already on the \code{cal BP} scale use \code{cc = 0}.
#' @param seed Set see to reproduce results. This seed is used for \code{C++}
#'     executions, if it is not assigned then the seed is set by the system.
#' @param alt_depths List of arrays with new depths.
#' @param quiet Boolean to hide status messages.
#' @param acc Numeric vector with the accumulation rates to use for the
#'     scenarios. If passed, then \code{acc_step}, \code{acc_lower}, and
#'     \code{acc_upper} will be ignored.
#' @param acc_step Accumulation rate step. Used to create alternative
#'     scenarios.
#' @param acc_lower Accumulation rate lower bound. Used to create alternative
#'     scenarios.
#' @param acc_upper Accumulation rate upper bound. Used to create alternative
#'     scenarios.
#' @param thick Numeric vector with the core segments' thickness to use for the
#'     scenarios. If passed, then \code{thick_step}, \code{thick_lower}, and
#'     \code{thick_upper} will be ignored.
#' @param thick_step Core segments thickness step. Used to create alternative
#'     scenarios.
#' @param thick_lower Core segments thickness lower bound. Used to create
#'     alternative scenarios.
#' @param thick_upper Core segments thickness upper bound. Used to create
#'     alternative scenarios.
#' @param dry_run Boolean flag to show (\code{dry_run = TRUE}) the scenarios
#'     that would be run with the current set of parameters, without actually
#'     running them.
#' @param restart Boolean flag to indicate if the execution should be resume
#'     from a previous one.
#' @param max_scenarios Numeric value with the maximum number of scenarios to
#' execute.
# @param ... Optional parameters for \code{\link[rbacon:Bacon]{rbacon::Bacon}}.
#' @inheritDotParams rbacon::Bacon -core -thick -coredir -seed -depths.file
#' -acc.mean -acc.shape -postbomb -hiatus.depths -cc -suggest -ask -ssize -th0
#' -plot.pdf
#'
#' @return List with \code{ggplot2} objects and summary statistics of all the
#'     scenarios computed.
#' @export
Bacon <- function(wdir,
                  entity,
                  cpus = 1,
                  postbomb = 0,
                  cc = 0,
                  seed = NA,
                  alt_depths = NULL,
                  quiet = FALSE,
                  acc = NULL,
                  acc_step = 5,
                  acc_lower = NULL,
                  acc_upper = NULL,
                  thick = NULL,
                  thick_step = 5,
                  thick_lower = NULL,
                  thick_upper = NULL,
                  dry_run = FALSE,
                  restart = FALSE,
                  max_scenarios = 100,
                  ...) {
  tictoc::tic(entity)
  wdir <- absolute_path(wdir)
  msg("Checking input files", quiet)
  check_files(wdir, entity)
  msg("Loading input files", quiet)
  path <- file.path(wdir, entity, 'Bacon_runs', entity)
  depths_eval <- matrix(read.table(file.path(path,
                                             paste0(entity, "_depths.txt")),
                                   col.names = ""))[[1]]
  sample_ids <-  read.csv(file.path(path, paste0(entity, "_sample_ids.csv")),
                         header = TRUE,
                         stringsAsFactors = FALSE,
                         colClasses = c("numeric"))

  core <- read.csv(file.path(path, paste0(entity, ".csv")),
                   header = TRUE,
                   stringsAsFactors = FALSE)

  path <- file.path(wdir, entity)
  unknown_age <- read.csv(file.path(path, "not_used_dates.csv"), header = TRUE)
  hiatuses <- read.csv(file.path(path, file.path("hiatus.csv")),
                       header = TRUE,
                       stringsAsFactors = FALSE,
                       colClasses = c("numeric", "numeric"))

  msg("Setting up environment", quiet)
  if (is.null(acc)) {
    accMean <- sapply(c(1, 2, 5), function(x) x * 10^(-1:2))
    ballpacc <- lm(core[, 2] * 1.1 ~ core[, 4])$coefficients[2]
    ballpacc <- abs(accMean - ballpacc)
    ballpacc <- ballpacc[ballpacc > 0]
    accMean <- sce_seq(accMean[order(ballpacc)[1]],
                       step = acc_step,
                       lower = acc_lower,
                       upper = acc_upper)
  } else {
    accMean <- acc
  }

  if (is.null(thick)) {
    # Calculate optimal thickness for each segment of the core
    k <- seq(floor(min(depths_eval, na.rm = TRUE)),
             ceiling(max(depths_eval, na.rm = TRUE)),
             by = 5)
    if (k[1] < 10) {
      thickness <- pretty(5 * (k/10), 10)
      thickness <- min(thickness[thickness > 0])
    } else if (k[1] > 20) {
      thickness <- max(pretty(5 * (k/20)))
    } else {
      thickness <- 5 # Default thickness
    }

    # Create range of thickness for alternative scenarios
    # if (is.null(thick_lower))
    #   thick_lower <- min(k)
    # if (is.null(thick_upper))
    #   thick_upper <- max(k)
    thickness <- sce_seq(thickness,
                         step = thick_step,
                         lower = thick_lower,
                         upper = thick_upper)
  } else {
    thickness <- thick
  }

  # Create sub-directories for each scenario
  scenarios <- data.frame(acc.mean = accMean,
                          thick = rep(thickness, each = length(accMean)))

  # Check the number of scenarios does not exceed the threshold, max_scenarios
  if (nrow(scenarios) > max_scenarios) {
    warning("The number of scenarios, exceeds the threshold of ",
            max_scenarios,
            ". If you are sure you want to proceed, then set max_scenarios > ",
            nrow(scenarios),
            call. = FALSE)
    return(invisible(list()))
  }

  if (dry_run) {
    message("The following scenarios will be executed: ")
    print(knitr::kable(scenarios,
                       col.names = c("Accumulation rate", "Thickness")))
    message("A total of ", nrow(scenarios), " scenarios.")
    return(invisible(scenarios))
  }
  wd0 <- getwd()
  setwd(file.path(wdir, entity))
  for (i in seq_len(nrow(scenarios))) {
    sce_name <- sprintf("S%03d-AR%03d-T%d", i, scenarios[i, 1], scenarios[i, 2])
    dir.create(file.path(wdir, entity, sce_name, entity),
               showWarnings = FALSE,
               recursive = TRUE)
    path0 <- file.path("../../Bacon_runs", entity)
    path1 <- file.path(sce_name, entity)
    filenames <- paste0(entity, c(".csv", "_sample_ids.csv", "_depths.txt"))
    . <- lapply(filenames, function(x) {
      sym_link(from = file.path(path0, x),
               to = file.path(path1, x))
    })
  }
  setwd(wd0)

  # Run scenarios in parallel
  # Detect the number of available CPUs
  avail_cpus <- parallel::detectCores() - 1
  cpus <- ifelse(cpus > avail_cpus, avail_cpus, cpus)

  msg("Running Bacon", quiet, nl = FALSE)

  # # Start parallel backend
  # log_file <- file.path(wdir, paste0("log-", entity,".txt"))
  # if (file.exists(log_file))
  #   file.remove(log_file)
  # cl <- parallel::makeCluster(cpus,
  #                             outfile = log_file)
  # doSNOW::registerDoSNOW(cl)
  # on.exit(parallel::stopCluster(cl)) # Stop cluster
  doFuture::registerDoFuture()
  oplan <- future::plan(future::multisession, workers = cpus)
  on.exit(future::plan(oplan), add = TRUE)
  oopt <- options(future.rng.onMisuse = "ignore") #,
                  # doFuture.foreach.export = ".export-and-automatic-with-warning")
  on.exit(options(oopt), add = TRUE)

  idx <- seq_len(nrow(scenarios))

  # # Set up progress bar
  # # pb <- txtProgressBar(max = length(idx), style = 3)
  # pb <- progress::progress_bar$new(
  #   format = "(:current/:total) [:bar] :percent",
  #   total = length(idx), clear = TRUE, width = 80)
  #
  # progress <- function(n) if (!quiet) pb$tick() # setTxtProgressBar(pb, n)
  # opts <- list(progress = progress)
  # Set up progress API
  p <- progressr::progressor(along = idx)

  # out <- foreach::foreach (i = idx,
  #                          .options.snow = opts) %dopar% {
  out <- foreach::foreach(i = idx,
                          .export = c("core"),
                          .verbose = FALSE) %dopar% {
    coredir <- sprintf("S%03d-AR%03d-T%d", i, scenarios[i, 1], scenarios[i, 2])
    msg(coredir)
    if (restart && is.done(file.path(wdir, entity, coredir, entity), entity)) {
      msg("Attempting to restart execution...")
      path <- file.path(wdir, entity, coredir, entity)

      if (file.exists(file.path(path, "alt_age_depth_plot.csv")) &&
          file.exists(file.path(path, "calib_ages_core.csv"))) {
        core <- read.csv(file.path(path, "calib_ages_core.csv"))
        df <- read.csv(file.path(path, "alt_age_depth_plot.csv"))
        alt_plot <- plot_age_depth(df,
                                   core = core,
                                   entity = entity,
                                   hiatuses = hiatuses)
        return(alt_plot)
      } else if (file.exists(file.path(path, "bacon_chronology.csv")) &&
                 file.exists(file.path(path, "calib_ages_core.csv"))) {
        core <- read.csv(file.path(path, "calib_ages_core.csv"))
        bacon_chronology <- read.csv(file.path(path, "bacon_chronology.csv"))
        df <- data.frame(x = bacon_chronology$depths,
                         y = bacon_chronology$median,
                         q5 = bacon_chronology$median +
                           bacon_chronology$uncert_5,
                         q95 = bacon_chronology$median -
                           bacon_chronology$uncert_95)
        alt_plot <- plot_age_depth(df,
                                   core = core,
                                   entity = entity,
                                   hiatuses = hiatuses)
        return(alt_plot)
      } else if (restart) {
          warning("Could not restart the execution of the model. \n",
                  "Running Bacon...",
                  call. = FALSE)
      }
    } else if (restart) {
        warning("Could not restart the execution of the model. \n",
                "Running Bacon...",
                call. = FALSE)
    }
    # Bacon log
    bacon_log <- file(paste0(coredir, ".log"), open = "wt")
    capture.output({
    # sink(file = paste0(coredir, ".log"))
    # sink(file = bacon_log, type = "output")
    # sink(file = bacon_log, type = "message")
    # oopt <- options(warn = -1)
    # on.exit(oopt, add = TRUE)
    output <- run_bacon(wdir = wdir,
                        entity = entity,
                        postbomb = postbomb,
                        cc = cc,
                        alt_depths = alt_depths,
                        quiet = quiet,
                        core = core,
                        seed = seed,
                        depths_eval = depths_eval,
                        hiatuses = hiatuses,
                        sample_ids = sample_ids,
                        unknown_age = unknown_age,
                        coredir = coredir,
                        acc.mean = scenarios[i, 1],
                        ssize = 2000,
                        th0 = c(),
                        thick = scenarios[i, 2],
                        close.connections = FALSE,
                        ...)
    }, file = bacon_log)
    # sink(type = "message")
    # sink(type = "output")
    close(bacon_log)
    # # sink()
    # sink()
    p()
    output
  }
  # Add new line after the progress bar
  if (!quiet) cat("\n")

  # Create output filename
  prefix <- paste0(entity, "_AR",
                   ifelse(length(accMean) > 1,
                          paste0(range(accMean), collapse = "-"),
                          accMean), "_T",
                   ifelse(length(thickness) > 1,
                          paste0(range(thickness), collapse = "-"),
                          thickness))

  # Create PDF with all the plots (age-depth)
  ggplot2::ggsave(filename = paste0(prefix, ".pdf"),
                  plot = plot_grid(out,
                                   scenarios,
                                   cond_x = "Acc. Rate",
                                   cond_y = "Thickness",
                                   cond_x_units = "yr/cm",
                                   cond_y_units = "cm",
                                   top = entity,
                                   left = "cal Age [yrs BP]",
                                   bottom = "Depth [mm]"),
                  device = "pdf",
                  path = wdir,
                  width = 7 * length(accMean),
                  height = 5 * length(thickness),
                  limitsize = FALSE)

  # Assess quality checks for the Bacon models
  idx <- seq_len(nrow(scenarios))
  accs <- vector("list", length = nrow(scenarios))
  abcs <- vector("list", length = nrow(scenarios))
  logs <- vector("list", length = nrow(scenarios))
  df_stats <- data.frame(matrix(nrow = nrow(scenarios), ncol = 4))
  colnames(df_stats) <- c("acc", "thick", "abc", "bias_rel")
  mcmcs <- vector("list", length = nrow(scenarios))
  pb <- progress::progress_bar$new(
    format = "(:current/:total) [:bar] :percent",
    total = length(idx), clear = TRUE, width = 80)
  if (!quiet)
    msg("Bacon QC", nl = FALSE)
  for (i in idx) {
    if (!quiet)
      pb$tick()
    coredir <- sprintf("S%03d-AR%03d-T%d", i, scenarios[i, 1], scenarios[i, 2])
    tmp <- bacon_qc(wdir = wdir,
                    entity = entity,
                    coredir = coredir,
                    acc.mean = scenarios[i, 1],
                    thick = scenarios[i, 2],
                    hiatuses = hiatuses)
    accs[[i]] <- tmp$acc
    abcs[[i]] <- tmp$abc
    logs[[i]] <- tmp$log
    df_stats[i, ] <- c(scenarios[i, 1], scenarios[i, 2], tmp$diff, tmp$bias)
    mcmcs[[i]] <- tmp$mcmc
    abc_chrono_dates[[i]] <- tmp$abc_chrono_dates
  }
  if (!quiet)
    cat("\n")

  # Save general stats
  idx <- with(df_stats, order(abc + abs(bias_rel)))
  write.csv(df_stats[idx, ],
            file.path(wdir, paste0(prefix, "-stats.csv")),
            row.names = FALSE)

  if (!quiet)
    msg(paste0("Best scenario: Acc. Rate = ",
               df_stats[idx, 1][1],
               "yr/cm - Thickness: ",
               df_stats[idx, 2][1],
               "cm"))

  # Create PDF with all the plots
  ## Accumulation Rate
  if (!quiet)
    msg("Plot Accumulation Rate")
  ggplot2::ggsave(filename = paste0(prefix, "-acc.pdf"),
                  plot = plot_grid(accs,
                                   scenarios,
                                   cond_x = "Acc. Rate",
                                   cond_y = "Thickness",
                                   cond_x_units = "yr/cm",
                                   cond_y_units = "cm",
                                   top = entity),
                  device = "pdf",
                  path = wdir,
                  width = 7 * length(accMean),
                  height = 5 * length(thickness),
                  limitsize = FALSE)
  ## Accumulation Rate Posterior and Prior difference
  if (!quiet)
    msg("Plot Accumulation Rate: Posterior vs Prior")
  ggplot2::ggsave(filename = paste0(prefix, "-acc-diff.pdf"),
                  plot = plot_grid(abcs,
                                   scenarios,
                                   cond_x = "Acc. Rate",
                                   cond_y = "Thickness",
                                   cond_x_units = "yr/cm",
                                   cond_y_units = "cm",
                                   append_title = TRUE,
                                   top = entity),
                  device = "pdf",
                  path = wdir,
                  width = 7 * length(accMean),
                  height = 5 * length(thickness),
                  limitsize = FALSE)
  ## Log posterior
  if (!quiet)
    msg("Plot Log Posterior (MCMC)")
  ggplot2::ggsave(filename = paste0(prefix, "-log.pdf"),
                  plot = plot_grid(logs,
                                   scenarios,
                                   cond_x = "Acc. Rate",
                                   cond_y = "Thickness",
                                   cond_x_units = "yr/cm",
                                   cond_y_units = "cm",
                                   append_title = TRUE,
                                   top = entity,
                                   left = "Log of Objective",
                                   bottom = "Iteration"),
                  device = "pdf",
                  path = wdir,
                  width = 7 * length(accMean),
                  height = 5 * length(thickness),
                  limitsize = FALSE)

  if (!quiet)
    msg("Bye!")
  tictoc::toc(quiet = quiet)
  return(list(ag = out,
              acc = accs,
              abc = abcs,
              log = logs,
              stats = df_stats[idx, ],
              mcmc = mcmcs,
              best_idx = idx[1]))
}

#' Run Bacon
#'
#' Run the function \code{rbacon:Bacon}{rbacon::Bacon(...)}.
#'
#' @importFrom grDevices dev.off pdf
#' @importFrom graphics abline arrows lines matplot points
#' @importFrom stats lm
#' @importFrom utils read.csv read.table write.csv write.table
#'
#' @param alt_depths List of arrays with new depths.
#' @param core Data frame with the core's data.
#' @param depths_eval Numeric array with the sampling depths.
#' @param hiatuses Data frame containing information of hiatuses.
#' @param sample_ids Numeric array with IDs for the sampling depths.
#' @param unknown_age Data frame containing information of unused ages.
#' @param coredir Folder where the core's files core are and/or will be located.
#' @param acc.mean The accumulation rate prior consists of a gamma distribution
#'     with two parameters. Its mean is set by acc.mean (default
#'     acc.mean=20 yr/cm (or whatever age or depth units are chosen), which can
#'     be changed to, e.g., 5, 10 or 50 for different kinds of deposits).
#'     Multiple values can be given in case of hiatuses or boundaries, e.g.,
#'     Bacon(hiatus.depths=23, acc.mean=c(5,20)).
#' @param acc.shape The prior for the accumulation rate consists of a gamma
#'     distribution with two parameters. Its shape is set by acc.shape
#'     (default acc.shape=1.5; higher values result in more peaked shapes).
#' @param ssize The approximate amount of iterations to store at the end of the
#'     MCMC run. Default 2000; decrease for faster (but less reliable) runs or
#'     increase for cores where the MCMC mixing (panel at upper-left corner of
#'     age-model graph) appears problematic.
#' @param th0 Starting years for the MCMC iterations.
#' @param thick Bacon will divide the core into sections of equal thickness
#'     specified by \code{thick} (default \code{thick = 5}).
#' @param p \code{progressor} object from the package
#'     \code{\link[progressr]{progressr}}.
#' @param ... Optional parameters for \code{\link[rbacon:Bacon]{rbacon::Bacon}}.
#' @inheritParams Bacon
#'
#' @return Saves MC ensemble, bacon_chronology and AM plot.
#'
#' @references
#' Blauuw, M. et al., Bayesian Analysis 6, 457-474 (2011)
#'
#' Blauuw, M. et al., rbacon (2019), R package version 2.3.9.1
#'
#' Comas-Bru, L. et al., SISALv2: A comprehensive speleothem isotope database
#' with multiple age-depth models, Earth Syst. Sci. Data Discuss (2020)
#' \url{https://doi.org/10.5194/essd-2020-39},
#' \url{https://github.com/paleovar/SISAL.AM}
#'
#' @keywords internal
run_bacon <- function(wdir,
                      entity,
                      postbomb = 0,
                      cc = 1,
                      seed = NA,
                      alt_depths = NULL,
                      quiet = FALSE,
                      core = NULL,
                      depths_eval = NULL,
                      hiatuses = NULL,
                      sample_ids = NULL,
                      unknown_age = NULL,
                      coredir = NULL,
                      acc.mean = 20,
                      acc.shape = 1.5,
                      ssize = 2000,
                      th0 = c(),
                      thick = 5,
                      p = NULL,
                      ...) {
  if (is.null(coredir))
    coredir <- "Bacon_runs"

  # Create path variable for Bacon inputs
  path <- file.path(wdir, entity, coredir, entity)

  # Create directory for plots
  dir.create(file.path(wdir, entity, "plots"), FALSE, TRUE)

  # msg("Running Bacon", quiet)
  hiatus.depths <- NA
  if (!is.null(hiatuses) && nrow(hiatuses) > 0)
    hiatus.depths <- hiatuses[, 2]
  tryCatch({
    pdf(file.path(path, paste0(entity, ".pdf")),
        width = 8,
        height = 6)
    rbacon::Bacon(core = entity,
                  thick = thick,
                  coredir = file.path(wdir, entity, coredir),
                  seed = seed,
                  depths.file = TRUE,
                  acc.mean = acc.mean,
                  acc.shape = acc.shape,
                  postbomb = postbomb,
                  hiatus.depths = hiatus.depths,
                  cc = cc,
                  suggest = FALSE,
                  ask = FALSE,
                  ssize = ssize,
                  th0 = th0,
                  plot.pdf = FALSE,
                  ...)
    dev.off()
    sym_link(from = file.path(path, paste0(entity, ".pdf")),
             to = file.path(wdir,
                            entity,
                            "plots",
                            paste0(entity, "-", coredir, ".pdf")))
  },
  error = function(e) {
    write.table(x = paste("ERROR in Bacon:", conditionMessage(e)),
                file = file.path(path, "bacon_error.txt"))
    stop(conditionMessage(e))
  })


  # List alternative depth files
  alt_depth_files <- list.files(path, "*_depths.alt.txt")
  if (!is.null(alt_depths)) {
    if (is.null(names(alt_depths))) {
      alt_depth_names <- paste0(entity,
                                 "alt_depth_",
                                 seq_len(length(alt_depths)))
    } else {
      alt_depth_names <- names(alt_depths)
    }
    for (i in seq_len(length(alt_depths))) {
      depths <- as.numeric(alt_depths[[i]])
      out <- rbacon::Bacon.hist(depths)
      depths <- cbind(depths, out)
      colnames(depths) <- c("depth", "min", "max", "median", "mean")
      write.csv(depths,
                file.path(path, paste0(alt_depth_names[i], ".csv")),
                row.names = FALSE)
    }
  } else if(length(alt_depth_files)) {
    for (i in seq_len(length(alt_depth_files))) {
      msg(alt_depth_files[i], quiet)
      depths <- matrix(read.table(file.path(path, alt_depth_files[i]),
                                  col.names = ""))[[1]]
      out <- rbacon::Bacon.hist(depths)
      depths <- cbind(depths, out)
      colnames(depths) <- c("depth", "min", "max", "median", "mean")
      new_name <- gsub(".alt.txt", "", alt_depth_files[i])
      write.csv(depths,
                file.path(path, paste0(new_name, ".csv")),
                row.names = FALSE)
    }
  }

  msg("Saving results", quiet)
  if (is.null(depths_eval))
    depths_eval <- matrix(read.table(file.path(path,
                                               paste0(entity, "_depths.txt")),
                                     col.names = ""))[[1]]
  bacon_mcmc <- sapply(depths_eval, rbacon::Bacon.Age.d)
  bacon_age_mean <- apply(bacon_mcmc, 2, mean)

  # 95% CI
  bacon_age <- get_bacon_median_quantile(depths_eval,
                                         NULL, # hiatuses,
                                         bacon_mcmc,
                                         q1 = 0.05,
                                         q2 = 0.95)
  colnames(bacon_age)[2:4] <- c("median", paste0("uncert_", c(5, 95)))
  bacon_age <- cbind(bacon_age[, 1], mean = bacon_age_mean, bacon_age[, 2:4])

  # 75% CI
  bacon_age_75 <- get_bacon_median_quantile(depths_eval,
                                            NULL, # hiatuses,
                                            bacon_mcmc,
                                            q1 = 0.25,
                                            q2 = 0.75)
  colnames(bacon_age_75)[3:4] <- paste0("uncert_", c(25, 75))
  bacon_mcmc <- rbind(depths_eval, bacon_mcmc)
  bacon_mcmc <- t(bacon_mcmc)
  bacon_mcmc <- cbind(sample_ids, bacon_mcmc)
  h <- NULL
  if (!is.null(hiatuses)) {
    h <- cbind(hiatuses,
               matrix(NA,
                      nrow = dim(hiatuses)[1],
                      ncol = dim(bacon_mcmc)[2] - 2))
    names(h) <- names(bacon_mcmc)
  }


  bacon_mcmc <- bacon_mcmc[order(bacon_mcmc[, 2]), ]
  sample_ids <- bacon_mcmc[, 1]

  bacon_mcmc <- rbind(bacon_mcmc, h)
  bacon_mcmc <- bacon_mcmc[order(bacon_mcmc[, 2]), ]
  # sample_ids <- bacon_mcmc[, 1]

  write.table(bacon_mcmc,
              file.path(path, "mc_bacon_ensemble.txt"),
              col.names = FALSE,
              row.names = FALSE)
  # Excluding hiatuses
  chronology <- cbind(sample_ids, bacon_age, bacon_age_75[, 3:4])
  colnames(chronology)[2] <- "depths"
  write.csv(chronology,
            file.path(path, "bacon_chronology.csv"),
            row.names = FALSE)

  if (is.null(core))
    core <- read.csv(file.path(path, paste0(entity, ".csv")))
  core$col <- "#E69F00"
  if (!is.null(unknown_age) && nrow(unknown_age) > 0) {
    unknown_age$col <- "#56B4E9"
    core <- rbind(core, unknown_age)
  }
  out <- rbacon::Bacon.hist(core$depth, draw = FALSE)
  # dput(out)
  core$age <- out[, 3]
  core$age_min <- out[, 1]
  core$age_max <- out[, 2]
  core$col[core$age <= 0] <- "#008060"
  write.csv(core,
            file.path(path, "calib_ages_core.csv"),
            row.names = FALSE)
  # print({
  #   rbacon::accrate.age.ghost()
  #   rbacon::agedepth(verbose = TRUE)
  # })
  chronology <- as.data.frame(chronology)
  df <- data.frame(x = chronology$depths,
                   y = chronology$median,
                   q5 = chronology$median + chronology$uncert_5,
                   q95 = chronology$median - chronology$uncert_95)
  write.csv(df,
            file.path(path, "alt_age_depth_plot.csv"),
            row.names = FALSE)

  alt_plot <- plot_age_depth(df,
                             core = core,
                             entity = entity,
                             hiatuses = hiatuses)
  ggplot2::ggsave(file.path(path, "final_age_model_alt.pdf"),
                  alt_plot,
                  width = 8,
                  height = 6)

  sym_link(from = file.path(path, "final_age_model_alt.pdf"),
           to = file.path(wdir,
                          entity,
                          "plots",
                          paste0(entity, "_ALT-", coredir, ".pdf")))
  # print(alt_plot)
  # set <- get('info')
  # return(set)
  done(path, entity)
  if (!is.null(p)) # Signal progress
    p()
  return(alt_plot)
}

#' Bacon quality control
#'
#' @inheritParams run_bacon
#' @return List with plots and numerical quality values.
#' @keywords internal
bacon_qc <- function(wdir,
                     entity,
                     core = NULL,
                     coredir = NULL,
                     acc.mean = 20,
                     acc.shape = 1.5,
                     thick = 5,
                     hiatuses = NULL) {
  if (is.null(coredir))
    coredir <- "Bacon_runs"

  # Create path variable for Bacon outputs
  path <- file.path(wdir, entity, coredir, entity)

  if (is.null(core)) {
    core <- read.csv(file.path(path, paste0(entity, ".csv")),
                     header = TRUE,
                     stringsAsFactors = FALSE)
  }
  max_depth <- max(core[, 4])
  K <- find_K(floor(max_depth/thick) + 1, path, entity)
  if (!file.exists(file.path(path, paste0(entity, "_", K, ".out")))) {
    print(wdir)
    print(entity)
    print(coredir)
    print(file.path(path, paste0(entity, "_", K, ".out")))
    return(list(
      acc = NULL,
      abc = NULL,
      log = NULL,
      diff = NA,
      var = NA
    ))
  }
  mcmc <- read.table(file.path(path, paste0(entity, "_", K, ".out")))
  out_acc <- plot_acc(K,
                      mcmc[, -ncol(mcmc)],
                      acc.mean,
                      acc.shape,
                      thick,
                      hiatuses,
                      plot = FALSE)
  out_abc <- plot_abc(out_acc$data, plot = FALSE)
  out_log <- plot_log_post(mcmc[, ncol(mcmc)])#, 0.1)
  abc_chrono_dates <- abc_chrono_ages(path)
  return(list(acc = out_acc$plot,
              abc = out_abc$plot,
              abc_chrono_dates = abc_chrono_dates,
              log = out_log$plot,
              diff = out_abc$abc,
              bias = out_log$bias,
              bias_rel = out_log$bias_rel,
              mcmc = mcmc))
}

#' Area Between Curves: Chronology and dates
#'
#' Find the area between the chronology curved and the original dates.
#'
#' @param path String with path were the bacon outputs are located.
#' @param sample_size Integer with the number of samples to use to find the area
#' between the chronology and original dates curves.
#' @param use_median Boolean flag to indicate which outputs of the chronology
#' should be used, `use_median = TRUE` uses the `median`, otherwise use `mean`.
#'
#' @return Numeric value with the area between curves
#' @keywords internal
#' @noRd
#'
#' @importFrom stats approxfun
abc_chrono_ages <- function(path, sample_size = 1000, use_median = TRUE) {
  files <- list.files(path, recursive = TRUE, full.names = TRUE)
  csv_files <- stringr::str_subset(files, "\\.csv$")
  suppressMessages({
  bacon_chrono <- csv_files %>%
    stringr::str_subset("bacon_chronology") %>%
    purrr::map_df(~suppressMessages(readr::read_csv(.x)))
  dates <- csv_files %>%
    # stringr::str_subset("alt_age_depth_plot|bacon_chronology|calib_ages_core|_sample_ids", negate = TRUE) %>%
    stringr::str_subset("alt_age_depth_plot", negate = TRUE) %>%
    stringr::str_subset("bacon_chronology", negate = TRUE) %>%
    stringr::str_subset("calib_ages_core", negate = TRUE) %>%
    stringr::str_subset("_sample_ids", negate = TRUE) %>%
    purrr::map_df(~suppressMessages(readr::read_csv(.x)))
  })

  fx1 <- NULL
  if (use_median) {
    fx1 <- approxfun(bacon_chrono$depths, bacon_chrono$median, na.rm = TRUE)
  } else {
    fx1 <- approxfun(bacon_chrono$depths, bacon_chrono$mean, na.rm = TRUE)
  }
  fx2 <- approxfun(dates$depth, dates$age, na.rm = TRUE)
  range_depths <- range(c(bacon_chrono$depths, dates$depth))
  test <- seq(from = min(range_depths),
              to = max(range_depths),
              length.out = sample_size)
  sum(fx2(test) - fx1(test), na.rm = TRUE)
}

#' Gelman-Rubin test
#'
#' Perform a Gelman and Rubin reduction Factor test.
#'
#' @param data List with MCMC runs output.
#' @param confidence Confidence level.
#'
#' @return Gelman and Rubin reduction factor.
#' @keywords internal
#'
gelman_test <- function(data, confidence = 0.975) {
  if (class(data) != "list")
    stop("Input must be a list of MCMC runs", call. = FALSE)
  # Find length of shortest run
  r <- min(unlist(lapply(data, nrow)))
  c <- min(unlist(lapply(data, ncol)))
  # Trim runs to have same length
  for (i in seq_len(length(data))) {
    data[[i]] <- data[[i]][1:r, 1:c]
  }
  out <- coda::gelman.diag(coda::mcmc.list(lapply(data, coda::as.mcmc)),
                           autoburnin = FALSE,
                           transform = TRUE,
                           confidence = confidence)
  return(out$mpsrf)
}

#' Create a mixed calibration curved
#'
#' @inheritParams IntCal::mix.curves
#' @inheritParams Bacon
#'
#' @export
#'
#' @examples
#' # Curve for neotropics
#' ageR::mix_curves(0.5, 1, 3, name = "neotropics.14C")
#' # Curve for coastline (Northern hemisphere)
#' ageR::mix_curves(0.7, 1, 2, name = "nh_coastal.14C")
#' # Curve for coastline (Southern hemisphere)
#' ageR::mix_curves(0.7, 3, 2, name = "sh_coastal.14C")
#' # Clean output
#' unlink("ccurves", TRUE, TRUE)
mix_curves <- function(proportion = 0.5,
                       cc1 = 1,
                       cc2 = 2,
                       name = "mixed.14C",
                       dirname = file.path(getwd(), "ccurves"),
                       quiet = FALSE) {
  if (!dir.exists(dirname)) # Create output directory
    dir.create(dirname, showWarnings = FALSE, recursive = TRUE)
  # Extract the IntCal20 calibration curves from IntCal
  cc1_df <- IntCal::copyCalibrationCurve(1)
  cc2_df <- IntCal::copyCalibrationCurve(2)
  cc3_df <- IntCal::copyCalibrationCurve(3)

  # Calibration curve names
  ccnames <- c("3Col_intcal20.14C",
               "3Col_marine20.14C",
               "3Col_shcal20.14C")
  # Calibration curve paths
  ccpaths <- file.path(dirname, ccnames)

  # Delete old calibration curves
  idx <- unlist(lapply(ccpaths, file.exists))
  . <- lapply(ccpaths[idx], file.remove)

  # Save the calibration curves
  write.table(cc1_df, ccpaths[1], row.names = FALSE, col.names = FALSE)
  write.table(cc2_df, ccpaths[2], row.names = FALSE, col.names = FALSE)
  write.table(cc3_df, ccpaths[3], row.names = FALSE, col.names = FALSE)

  # Create a mixed calibration curve
  IntCal::mix.curves(proportion = proportion,
                     cc1 = ccnames[cc1],
                     cc2 = ccnames[cc2],
                     name = name,
                     dirname = dirname)
  if (!quiet)
    msg(paste0("Mixed curved: ",
               proportion * 100, "/", (1 - proportion) * 100,
               " created."))
}

#' Age model function for linear regression.
#'
#' @param wdir path where input files are stored.
#' @param entity name of the entity.
#' @param N number of iterations.
#' @return saves MC ensemble, lin_reg_chronology and AM plot.
#' @export
#' @references
#' Telford, R. J. et al., Quaternary Science Reviews 23, 1-5 (2004)
#'
#' Comas-Bru, L. et al., SISALv2: A comprehensive speleothem isotope database
#' with multiple age-depth models, Earth Syst. Sci. Data Discuss (2020)
#' \url{https://doi.org/10.5194/essd-2020-39},
#' \url{https://github.com/paleovar/SISAL.AM}
runLinReg <- function(wdir, entity, N = 2000) {
  # Local binding
  sample_id <- depth_eval <- NULL

  print("---------------- Read in data -------------")
  setwd(file.path(wdir, entity, "linReg"))
  dating_tb <- read.csv(paste0(entity, ".csv"),
                        header = TRUE,
                        stringsAsFactors = FALSE)
  depth_sample <- read.csv(paste0(entity, "_depths.csv"),
                           header = TRUE,
                           stringsAsFactors = FALSE,
                           colClasses = c("numeric", "numeric"))
  id <-read.csv(paste0(entity, "_ids.csv"),
                header = TRUE,
                stringsAsFactors = FALSE,
                colClasses = c("numeric", "numeric"))

  setwd(file.path(wdir, entity))
  hiatuses <- read.csv("hiatus.csv",
                       header = TRUE,
                       stringsAsFactors = FALSE,
                       colClasses = c("numeric", "numeric"))
  unknown_age <- read.csv("not_used_dates.csv", header = TRUE)

  sample <- data.frame(sample_id = id[, 1],#$sample_id,
                       depth_eval = id[, 2]) #id$depth_sample)

  print("------- MC Simulations----------")
  mc_runs <- mc_ensemble(linReg = TRUE,
                         age = dating_tb[, 2], #$corr_age,
                         age_error = dating_tb[, 3], #$corr_age_uncert,
                         N = 2000,
                         wdir = wdir,
                         entity = entity)
  print("--------lin Reg ------------")
  N <- dim(mc_runs)[1]
  lr <- mc_linReg(N,
                  hiatuses[, 2], # $depth_sample,
                  dating_tb[, 4], #$depth_dating,
                  mc_runs,
                  depth_sample[, 2]) #$depth_sample)

  lr <-  merge(sample,
               lr,
               by = "depth_eval",
               all.x = TRUE,
               all.y = TRUE)
  lr <- dplyr::select(lr, sample_id, depth_eval, dplyr::everything())

  print("---------------save data Lin Reg --------------")
  setwd(file.path(wdir, entity, "/linReg"))
  write.table(as.matrix(mc_runs),
              "dating_mc_linReg_ensemble.txt",
              col.names = FALSE,
              row.names = FALSE)
  write.table(as.matrix(lr),
              "mc_linReg_ensemble.txt",
              col.names = FALSE,
              row.names = FALSE)

  print("--------------median and quantiles--------------")
  stats <- get_median_quantiles(lr[, 3:N + 2], q1 = 0.05, q2 = 0.95)
  age_median <- stats[, 1]
  age_sd_low <- stats[, 2]
  age_sd_high <- stats[, 3]
  lin_reg <- cbind(lr$sample_id,
                   age_median,
                   age_sd_high - age_median,
                   age_median - age_sd_low)
  colnames(lin_reg) <- c("sample_id",
                         "lin_reg_age",
                         "lin_reg_age_uncert_pos",
                         "lin_reg_age_uncert_neg")
  write.csv(lin_reg,
            "linReg_chronology.csv",
            row.names = FALSE)

  pdf("final_age_model.pdf", 6, 4)
  matplot(x = age_median,
          y = lr$depth_eval,
          col = "black",
          lty = 1,
          type = "l",
          lwd = 1,
          ylim = c(max(lr$depth_eval), 0),
          xlab = "Age [yrs BP]",
          ylab = "Depth from top [mm]")
  lines(x = age_sd_high,
        y = lr$depth_eval,
        lty = 2,
        col = "red")
  lines(x = age_sd_low,
        y = lr$depth_eval,
        lty = 2,
        col = "red")
  points(x = dating_tb[, 2], #$corr_age,
         y = dating_tb[, 4], #$depth_dating,
         lty = 2,
         col = "orange",
         pch = 4)
  arrows(dating_tb[, 2] - dating_tb[, 3],
         dating_tb[, 4],
         dating_tb[, 2] + dating_tb[, 3],
         dating_tb[, 4],
         length = 0.05,
         angle = 90,
         code = 3,
         col = "orange")
  if (!plyr::empty(data.frame(hiatuses))) {
    abline(h = hiatuses[, 2], #$depth_sample,
           col = "grey",
           lty = 2)
  }
  dev.off()
}
