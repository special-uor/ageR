#' Age model function for Bacon
#'
#' @importFrom foreach `%dopar%`
#'
#' @param wdir Path where input files are stored.
#' @param entity Name of the entity.
#' @param cpus Number of CPUs to be used on the computation of the age models.
#' @param postbomb Use a postbomb curve for negative (i.e. postbomb) 14C ages.
#'     0 = none, 1 = NH1, 2 = NH2, 3 = NH3, 4 = SH1-2, 5 = SH3
#' @param cc Calibration curve.
#' @param alt_depths List of arrays with new depths.
#' @param quiet Boolean to hide status messages.
#' @param acc_step Accumulation rate step. Used to create alternative
#'     scenarios.
#' @param acc_lower Accumulation rate lower bound. Used to create alternative
#'     scenarios.
#' @param acc_upper Accumulation rate upper bound. Used to create alternative
#'     scenarios.
#' @param thick_step Core segments thickness step. Used to create alternative
#'     scenarios.
#' @param thick_lower Core segments thickness lower bound. Used to create
#'     alternative scenarios.
#' @param thick_upper Core segments thickness upper bound. Used to create
#'     alternative scenarios.
#' @param dry_run Boolean flag to show (\code{dry_run = TRUE}) the scenarios
#'     that would be run with the current set of parameters, without actually
#'     running them.
#' @param ... Optional parameters for \code{\link[rbacon:Bacon]{rbacon::Bacon}}.
#'
#' @return
#' @export
#'
# @examples
Bacon <- function(wdir,
                  entity,
                  cpus = 1,
                  postbomb = 0,
                  cc = 0,
                  alt_depths = NULL,
                  quiet = FALSE,
                  acc_step = 5,
                  acc_lower = NULL,
                  acc_upper = NULL,
                  thick_step = 5,
                  thick_lower = NULL,
                  thick_upper = NULL,
                  dry_run = FALSE,
                  ...) {
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
  hiatus_tb <- read.csv(file.path(path, file.path("hiatus.csv")),
                        header = TRUE,
                        stringsAsFactors = FALSE,
                        colClasses = c("numeric", "numeric"))

  msg("Setting up environment", quiet)
  accMean <- sapply(c(1, 2, 5), function(x) x * 10^(-1:2))
  ballpacc <- lm(core[, 2] * 1.1 ~ core[, 4])$coefficients[2]
  ballpacc <- abs(accMean - ballpacc)
  ballpacc <- ballpacc[ballpacc > 0]
  accMean <- sce_seq(accMean[order(ballpacc)[1]],
                     step = acc_step,
                     lower = acc_lower,
                     upper = acc_upper)

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
  if (is.null(thick_lower))
    thick_lower <- min(k)
  if (is.null(thick_upper))
    thick_upper <- max(k)
  thickness <- sce_seq(thickness,
                       step = thick_step,
                       lower = thick_lower,
                       upper = thick_upper)

  # Create subfolders for each scenario
  scenarios <- data.frame(acc.mean = accMean,
                          thick = rep(thickness, each = length(accMean)))

  if (dry_run) {
    message("The following scenarios will be executed: ")
    print(
      knitr::kable(scenarios, col.names = c("Accumalation rate", "Thickness"))
    )
    message("A total of ", nrow(scenarios), " scenarios.")
    return(invisible())
  }
  wd0 <- getwd()
  setwd(file.path(wdir, entity))
  for (i in seq_len(nrow(scenarios))) {
    sce_name <- sprintf("S%03d-AR%03d-T%d", i, scenarios[i, 1], scenarios[i, 2])
    # print(file.path(wdir, entity, sce_name))
    dir.create(file.path(wdir, entity, sce_name, entity),
               showWarnings = FALSE,
               recursive = TRUE)
    path0 <- file.path("../../Bacon_runs", entity)
    path1 <- file.path(sce_name, entity)
    filenames <- paste0(entity, c(".csv", "_sample_ids.csv", "_depths.txt"))
    . <- lapply(filenames, function(x) {
      to <- file.path(path1, x)
      if (file.exists(to))
        file.remove(to)
      . <- file.symlink(from = file.path(path0, x),
                        to = to)
    })
  }
  setwd(wd0)

  # Run scenarios in parallel
  # Detect the number of available CPUs
  avail_cpus <- parallel::detectCores() - 1
  cpus <- ifelse(cpus > avail_cpus, avail_cpus, cpus)

  # Start parallel backend
  cl <- parallel::makeCluster(cpus, outfile = paste0("log-", entity, ".txt"))
  doSNOW::registerDoSNOW(cl)
  idx <- seq_len(nrow(scenarios))
  out <- foreach::foreach (i = idx) %dopar% {
    coredir <- sprintf("S%03d-AR%03d-T%d", i, scenarios[i, 1], scenarios[i, 2])
    msg(coredir)
    out <- runBacon(wdir = wdir,
                    entity = entity,
                    postbomb = postbomb,
                    cc = cc,
                    alt_depths = alt_depths,
                    quiet = quiet,
                    core = core,
                    depths_eval = depths_eval,
                    hiatus_tb = hiatus_tb,
                    sample_ids = sample_ids,
                    unknown_age = unknown_age,
                    coredir = coredir,
                    acc.mean = scenarios[i, 1],
                    ssize = 2000,
                    th0 = c(),
                    thick = scenarios[i, 2],
                    close.connections = FALSE,
                    ...)
    out
  }
  parallel::stopCluster(cl) # Stop cluster

  # Remove labels
  for (i in seq_len(length(out))) {
    tmp <- out[[i]]
    idx <- arrayInd(i, c(length(accMean), length(thickness)))
    idx_x <- idx[, 1]
    idx_y <- idx[, 2]
    tmp$labels$x <- NULL
    if (idx_x == 1) {
      tmp$labels$y <- paste0("Thickness: ", thickness[idx_y], "cm")
    } else {
      tmp$labels$y <- NULL
    }
    tmp$labels$title <- paste0("Acc. Rate: ", accMean[idx_x], "yr/cm")
    out[[i]] <- tmp
  }

  # Create output filename
  allplots <- paste0(entity, "_AR",
                     ifelse(length(accMean) > 1,
                            paste0(range(accMean), collapse = "-"),
                            accMean), "_T",
                     ifelse(length(thickness) > 1,
                            paste0(range(thickness), collapse = "-"),
                            thickness))

  # Create PDF with all the plots
  pdf(file.path(wdir, paste0(allplots, ".pdf")),
      width = 7 * length(accMean),
      height = 5 * length(thickness))
  gridExtra::grid.arrange(grobs = out,
                          nrow = length(thickness),
                          top = entity,
                          left = "cal Age [yrs BP]",
                          bottom = "Depth from top [mm]")
  dev.off()
  save(out,
       file = file.path(wdir, paste0(allplots, ".RData")))
       #paste0(entity, "-plots.RData"))
  # return(out)
}

#' Run Bacon.
#'
#' @importFrom grDevices dev.off
#' @importFrom grDevices pdf
#' @importFrom graphics abline
#' @importFrom graphics arrows
#' @importFrom graphics lines
#' @importFrom graphics matplot
#' @importFrom graphics points
#' @importFrom stats lm
#' @importFrom utils read.csv
#' @importFrom utils read.table
#' @importFrom utils write.csv
#' @importFrom utils write.table
#'
#' @param wdir Path where input files are stored.
#' @param entity Name of the entity.
#' @param postbomb Use a postbomb curve for negative (i.e. postbomb) 14C ages.
#'     0 = none, 1 = NH1, 2 = NH2, 3 = NH3, 4 = SH1-2, 5 = SH3
#' @param cc Calibration curve.
#' @param alt_depths List of arrays with new depths.
#' @param quiet Boolean to hide status messages.
#' @param core Data frame with the core's data.
#' @param depths_eval Numeric array with the sampling depths.
#' @param hiatus_tb Data frame containing information of hiatuses.
#' @param sample_ids Numeric array with IDs for the sampling depths.
#' @param unknown_age Data frame containing information of unused ages.
#' @param coredir Folder where the core's files core are and/or will be located.
#' @param acc.mean The accumulation rate prior consists of a gamma distribution
#'     with two parameters. Its mean is set by acc.mean (default
#'     acc.mean=20 yr/cm (or whatever age or depth units are chosen), which can
#'     be changed to, e.g., 5, 10 or 50 for different kinds of deposits).
#'     Multiple values can be given in case of hiatuses or boundaries, e.g.,
#'     Bacon(hiatus.depths=23, acc.mean=c(5,20)).
#' @param ssize The approximate amount of iterations to store at the end of the
#'     MCMC run. Default 2000; decrease for faster (but less reliable) runs or
#'     increase for cores where the MCMC mixing (panel at upper-left corner of
#'     age-model graph) appears problematic.
#' @param th0 Starting years for the MCMC iterations.
#' @param thick Bacon will divide the core into sections of equal thickness
#'     specified by \code{thick} (default \code{thick = 5}).
#' @param ... Optional parameters for \code{\link[rbacon:Bacon]{rbacon::Bacon}}.
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
runBacon <- function(wdir,
                     entity,
                     postbomb = 0,
                     cc = 0,
                     alt_depths = NULL,
                     quiet = FALSE,
                     core = NULL,
                     depths_eval = NULL,
                     hiatus_tb = NULL,
                     sample_ids = NULL,
                     unknown_age = NULL,
                     coredir = NULL,
                     acc.mean = 20,
                     ssize = 2000,
                     th0 = c(),
                     thick = 5,
                     ...) {
  if (is.null(coredir))
    coredir <- "Bacon_runs"

  # Create path variable for Bacon inputs
  path <- file.path(wdir, entity, coredir, entity)

  # Create directory for plots
  dir.create(file.path(wdir, entity, "plots"), FALSE, TRUE)

  msg("Running Bacon", quiet)
  if (nrow(hiatus_tb) == 0) {
    tryCatch({
      pdf(file.path(path, paste0(entity, ".pdf")),
          width = 8,
          height = 6)
      rbacon::Bacon(core = entity,
                    thick = thick,
                    coredir = file.path(wdir, entity, coredir),
                    depths.file = TRUE,
                    acc.mean = acc.mean,
                    postbomb = postbomb,
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
  } else {
    tryCatch({
      pdf(file.path(path, paste0(entity, ".pdf")),
          width = 8,
          height = 6)
      rbacon::Bacon(core = entity,
                    thick = thick,
                    coredir = file.path(wdir, entity, coredir),
                    depths.file = TRUE,
                    acc.mean = acc.mean,
                    postbomb = postbomb,
                    hiatus.depths = hiatus_tb[, 2],
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
  }

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
  bacon_mcmc <- sapply(depths_eval, rbacon::Bacon.Age.d)
  # 95% CI
  bacon_age <- get_bacon_median_quantile(depths_eval,
                                         hiatus_tb,
                                         bacon_mcmc,
                                         q1 = 0.05,
                                         q2 = 0.95)
  colnames(bacon_age)[3:4] <- paste0("uncert_", c(5, 95))
  # 75% CI
  bacon_age_75 <- get_bacon_median_quantile(depths_eval,
                                            hiatus_tb,
                                            bacon_mcmc,
                                            q1 = 0.25,
                                            q2 = 0.75)
  colnames(bacon_age_75)[3:4] <- paste0("uncert_", c(25, 75))
  bacon_mcmc <- rbind(depths_eval, bacon_mcmc)
  bacon_mcmc <- t(bacon_mcmc)
  bacon_mcmc <- cbind(sample_ids, bacon_mcmc)

  h <- cbind(hiatus_tb,
             matrix(NA,
                    nrow = dim(hiatus_tb)[1],
                    ncol = dim(bacon_mcmc)[2] - 2))
  names(h) <- names(bacon_mcmc)

  bacon_mcmc <- rbind(bacon_mcmc, h)
  bacon_mcmc <- bacon_mcmc[order(bacon_mcmc[, 2]), ]

  sample_ids <- bacon_mcmc[, 1]

  write.table(bacon_mcmc,
              file.path(path, "mc_bacon_ensemble.txt"),
              col.names = FALSE,
              row.names = FALSE)
  chronology <- cbind(sample_ids, bacon_age, bacon_age_75[, 3:4])
  colnames(chronology)[2] <- "depths"
  write.csv(chronology,
            file.path(path, "bacon_chronology.csv"),
            row.names = FALSE)

  core$col <- "#E69F00"
  if (!is.null(unknown_age) && nrow(unknown_age) > 0) {
    unknown_age$col <- "#56B4E9"
    core <- rbind(core, unknown_age)
  }
  out <- rbacon::Bacon.hist(core$depth)
  core$age <- out[, 3]
  # print({
  #   rbacon::accrate.age.ghost()
  #   rbacon::agedepth(verbose = TRUE)
  # })
  df <- data.frame(x = bacon_age[, 1] * 10,
                   y = bacon_age[, 2],
                   q5 = bacon_age[, 2] + bacon_age[, 3],
                   q95 = bacon_age[, 2] - bacon_age[, 4])
  alt_plot <- plot_age_depth(df,
                             core = core,
                             entity = entity,
                             hiatuses = hiatus_tb)
  ggplot2::ggsave(file.path(path, "final_age_model_alt.pdf"),
                  alt_plot,
                  width = 8,
                  height = 6)

  sym_link(from = file.path(path, "final_age_model_alt.pdf"),
           to = file.path(wdir,
                          entity,
                          "plots",
                          paste0(entity, "_ALT-", coredir, ".pdf")))
  # set <- get('info')
  # return(set)
  # pdf(file.path(path, "final_age_model.pdf"), 6, 4)
  # matplot(y = bacon_age[, 2],
  #         x = bacon_age[, 1] * 10,
  #         col = "black",
  #         lty = 1,
  #         type = "l",
  #         lwd = 1,
  #         xlim = c(0, max(depths_eval * 10)),
  #         ylab = "cal Age [yrs BP]",
  #         xlab = "Depth from top [mm]")
  # lines(y = bacon_age[, 3] + bacon_age[, 2],
  #       x = bacon_age[, 1] * 10,
  #       lty = 2,
  #       col = "red")
  # lines(y = bacon_age[, 2] - bacon_age[, 4],
  #       x = bacon_age[, 1] * 10,
  #       lty = 2,
  #       col = "red")
  # points(y = core[, 2],
  #   x = core[, 4] * 10,
  #   lty = 2,
  #   col = core$col,
  #   pch = 4)
  # arrows(y0 = core[, 2] - core[, 3],
  #        x0 = core[, 4] * 10,
  #        y1 = core[, 2] + core[, 3],
  #        x1 = core[, 4] * 10,
  #        length = 0.05,
  #        angle = 90,
  #        code = 3,
  #        col = core$col
  # )
  # if (!plyr::empty(data.frame(hiatus_tb))) {
  #   abline(h = hiatus_tb[, 2] * 10,
  #          col = "grey",
  #          lty = 2)
  # }
  # dev.off()
  return(alt_plot)
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
  hiatus_tb <- read.csv("hiatus.csv",
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
                  hiatus_tb[, 2], # $depth_sample,
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
  if (!plyr::empty(data.frame(hiatus_tb))) {
    abline(h = hiatus_tb[, 2], #$depth_sample,
           col = "grey",
           lty = 2)
  }
  dev.off()
}
