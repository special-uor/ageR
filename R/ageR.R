#' Age model function for Bacon.
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
#' @param wdir path where input files are stored.
#' @param entity name of the entity.
#' @param postbomb postbomb curve
#' @param cc calibration curve
#'
#' @return saves MC ensembel, bacon_chronology and AM plot.
#'
#' @export
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
runBacon <- function(wdir, entity, postbomb = 0, cc = 0) {
  # setwd(file.path(wdir, entity))
  # filenames <- file.path(c(file.path("Bacon_runs/",
  #                                    entity,
  #                                    c(paste0(entity, "_depths.txt"),
  #                                      paste0(entity, "_sample_ids.csv"),
  #                                      paste0(entity, ".csv"))),
  #                          "hiatus.csv",
  #                          "not_used_dates.csv"))
  # idx <- unlist(lapply(filenames, file.exists))
  # if (!all(idx)) {
  #   stop(paste0("\nThe following input ",
  #               ifelse(sum(!idx) > 1, "files were", "file was"),
  #               " not found inside the working directory [",
  #               file.path(entity),
  #               "]\n",
  #               paste0("- ", filenames[!idx], collapse = "\n")))
  # }
  check_files(wdir, entity)
  setwd(file.path(wdir, entity, "Bacon_runs", entity))
  depth_eval <- matrix(read.table(paste0(entity, "_depths.txt"),
                                col.names = ""))[[1]]
  sample_id <-  read.csv(paste0(entity, "_sample_ids.csv"),
                         header = TRUE,
                         stringsAsFactors = FALSE,
                         colClasses = c("numeric"))

  core <- read.csv(paste0(entity, ".csv"),
                   header = TRUE,
                   stringsAsFactors = FALSE,
                   colClasses = c("character", "numeric", "numeric", "numeric"))

  setwd(file.path(wdir, entity))
  unknown_age <- read.csv("not_used_dates.csv", header = TRUE)
  # if (file.exists(file.path("hiatus.csv"))) {
  hiatus_tb <- read.csv(file.path("hiatus.csv"),
                        header = TRUE,
                        stringsAsFactors = FALSE,
                        colClasses = c("numeric", "numeric"))
  # } else {
  #   hiatus_tb <- data.frame(sample_id = NA, depth_sample = NA)[-1, ]
  # }

  accMean<- sapply(c(1, 2, 5), function(x) x * 10^(-1:2))
  ballpacc <- lm(core[,2] * 1.1 ~ core[, 4])$coefficients[2]
  ballpacc <- abs(accMean - ballpacc)
  ballpacc <- ballpacc[ballpacc > 0]
  accMean <- accMean[order(ballpacc)[1]]

  k <- seq(floor(min(depth_eval, na.rm = TRUE)),
           ceiling(max(depth_eval, na.rm = TRUE)),
           by = 5)
  if (k[1] < 10) {
    thickness <- pretty(5 * (k/10), 10)
    thickness <- min(thickness[thickness > 0])
  } else if (k[1] > 20) {
    thickness <- max(pretty(5 * (k/20)))
  } else {
    thickness <- 5 # Default thickness
  }

  j <- 2000
  tho <- c()

  print("#------------ run bacon ---------------#")
  if (nrow(hiatus_tb) == 0) {
    tryCatch({
      # pdf(file.path(wdir, entity, "Bacon_runs", entity, paste0(entity, ".pdf")),
      #     width = 6,
      #     height = 6,
      #     )
      rbacon::Bacon(core = entity,
                    depths.file = TRUE,
                    thick = thickness,
                    acc.mean = accMean,
                    postbomb = postbomb,
                    cc = cc,
                    suggest = FALSE,
                    ask = FALSE,
                    ssize = j,
                    th0 = tho,
                    plot.pdf = TRUE)
      # dev.off()
    },
    error = function(e) {
      write.table(x = paste("ERROR in Bacon:", conditionMessage(e)),
                  file = "bacon_error.txt")
    })
  } else {
    tryCatch({
      pdf(file.path(wdir, entity, "Bacon_runs", entity, paste0(entity, ".pdf")),
          width = 6,
          height = 6)
      rbacon::Bacon(core = entity,
                    depths.file = TRUE,
                    thick = thickness,
                    acc.mean = accMean ,
                    postbomb = postbomb,
                    hiatus.depths = hiatus_tb[, 2],
                    cc = cc,
                    suggest = FALSE,
                    ask = FALSE,
                    ssize = j,
                    th0 = tho,
                    plot.pdf = FALSE)
      dev.off()
    },
    error = function(e) {
      write.table(x = paste("ERROR in Bacon:", conditionMessage(e)),
                  file = "bacon_error.txt")
    })
  }

  print("--------------save data -----------------")
  bacon_mcmc <- sapply(depth_eval, rbacon::Bacon.Age.d)
  bacon_age <- ageR::get_bacon_median_quantile(depth_eval,
                                               hiatus_tb,
                                               bacon_mcmc)
  bacon_mcmc <- rbind(depth_eval, bacon_mcmc)
  bacon_mcmc <- t(bacon_mcmc)
  bacon_mcmc <- cbind(sample_id, bacon_mcmc)

  h <- cbind(hiatus_tb,
             matrix(NA,
                    nrow = dim(hiatus_tb)[1],
                    ncol = dim(bacon_mcmc)[2] - 2))
  names(h) <- names(bacon_mcmc)

  bacon_mcmc <- rbind(bacon_mcmc, h)
  bacon_mcmc <- bacon_mcmc[order(bacon_mcmc[, 2]), ]

  sample_id <- bacon_mcmc[, 1]

  setwd(file.path(wdir, entity, "Bacon_runs", entity))
  write.table(bacon_mcmc,
              "mc_bacon_ensemble.txt",
              col.names = FALSE,
              row.names = FALSE)
  write.csv(cbind(sample_id, bacon_age[, 2:4]),
            "bacon_chronology.csv",
            row.names = FALSE)

  pdf("final_age_model.pdf", 6, 4)
  matplot(x = bacon_age[, 2],
          y = bacon_age[, 1] * 10,
          col = "black",
          lty = 1,
          type = "l",
          lwd = 1,
          ylim = c(max(depth_eval * 10), 0),
          xlab = "Age [yrs BP]",
          ylab = "Depth from top [mm]")
  lines(x = bacon_age[, 3] + bacon_age[, 2],
        y = bacon_age[, 1] * 10,
        lty = 2,
        col = "red")
  lines(x = bacon_age[, 2] - bacon_age[, 4],
        y = bacon_age[, 1] * 10,
        lty = 2,
        col = "red")
  points(x = core[, 2],
    y = core[, 4] * 10,
    lty = 2,
    col = "orange",
    pch = 4)
  arrows(core[, 2] - core[, 3],
         core[, 4] * 10,
         core[, 2] + core[, 3],
         core[, 4] * 10,
         length = 0.05,
         angle = 90,
         code = 3,
         col = "orange"
  )
  if (!plyr::empty(data.frame(hiatus_tb))) {
    abline(h = hiatus_tb[, 2] * 10,
           col = "grey",
           lty = 2)
  }
  dev.off()
}

#' Age model function for linear regression.
#'
#' @param wdir path where input files are stored.
#' @param entity name of the entity.
#' @param N number of iterations.
#' @return saves MC ensembel, lin_reg_chronology and AM plot.
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
