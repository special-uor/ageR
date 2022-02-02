#' @rdname Bacon
#' @export
Bacon2 <- function(wdir,
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

  # Start parallel backend
  oplan <- future::plan(future::multisession, workers = cpus)
  on.exit(future::plan(oplan), add = TRUE)
  oopt <- options(future.rng.onMisuse = "ignore")
  on.exit(options(oopt), add = TRUE)

  idx <- seq_len(nrow(scenarios))

  # Set up progress API
  p <- progressr::progressor(along = idx)
  out <- idx %>%
    furrr::future_map(
      function(i) {
        coredir <- sprintf("S%03d-AR%03d-T%d", i, scenarios[i, 1],
                           scenarios[i, 2])
        msg(coredir)
        if (restart &&
            is.done(file.path(wdir, entity, coredir, entity), entity)) {
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
            bacon_chronology <- read.csv(file.path(path,
                                                   "bacon_chronology.csv"))
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
        close(bacon_log)
        p()
        output
      }
    )

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
  # accs <- vector("list", length = nrow(scenarios))
  # abcs <- vector("list", length = nrow(scenarios))
  # logs <- vector("list", length = nrow(scenarios))
  # df_stats <- data.frame(matrix(nrow = nrow(scenarios), ncol = 4))
  # colnames(df_stats) <- c("acc", "thick", "abc", "bias_rel")
  mcmcs <- vector("list", length = nrow(scenarios))
  if (!quiet)
    msg("Bacon QC", nl = FALSE)
  p <- progressr::progressor(along = idx)
  output_qc <- idx %>%
    furrr::future_map_dfr(function(i) {
      if (!quiet) p()
      coredir <- sprintf("S%03d-AR%03d-T%d", i, scenarios[i, 1],
                         scenarios[i, 2])
      tmp <- bacon_qc(wdir = wdir,
                      entity = entity,
                      coredir = coredir,
                      acc.mean = scenarios[i, 1],
                      thick = scenarios[i, 2],
                      hiatuses = hiatuses)
      tibble::tibble(
        acc = scenarios[i, 1],
        thick = scenarios[i, 2],
        abc_chrono_ages =  tmp$abc_chrono_ages,
        abc = tmp$diff,
        bias_rel = tmp$bias,
        accs = list(tmp$acc),
        abcs = list(tmp$abc),
        logs = list(tmp$log),
        mcmcs =  list(tmp$mcmc)
      )
    })

  if (!quiet)
    cat("\n")

  # Save general stats
  df_stats <- output_qc %>%
    dplyr::select(1:5)
  idx <- with(df_stats, order(abc_chrono_ages, abc + abs(bias_rel)))
  write.csv(df_stats[idx, ],
            file.path(wdir, paste0(prefix, "-stats.csv")),
            row.names = FALSE)

  if (!quiet)
    msg(paste0("Best scenario: Acc. Rate = ",
               df_stats[idx, ]$acc[1],
               "yr/cm - Thickness: ",
               df_stats[idx, ]$thick[1],
               "cm"))

  # Create PDF with all the plots
  ## Accumulation Rate
  if (!quiet)
    msg("Plot Accumulation Rate")
  ggplot2::ggsave(filename = paste0(prefix, "-acc.pdf"),
                  plot = plot_grid(output_qc$accs,
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
                  plot = plot_grid(output_qc$abcs,
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
                  plot = plot_grid(output_qc$logs,
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
  return(output_qc[idx, ])
  # return(list(ag = out,
  #             acc = accs,
  #             abc = abcs,
  #             log = logs,
  #             stats = df_stats[idx, ],
  #             mcmc = mcmcs,
  #             best_idx = idx[1]))
}
