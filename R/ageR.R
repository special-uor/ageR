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
#' @param postbomb TODO
#' @param cc TODO
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
#' with multiple age-depth models, Earth Syst. Sci. Data Discuss (2019)
#' \url{https://github.com/paleovar/SISAL.AM}
runBacon <- function(wdir, entity, postbomb = 0, cc = 0) {
  setwd(file.path(wdir, "/Bacon_runs/", entity))
  depth_eval <- matrix(read.table(paste0(entity, "_depths.txt"),
                                col.names = ""))[[1]]
  sample_id <-  read.csv(paste0(entity, "_sample_ids.csv"),
                         header = TRUE,
                         stringsAsFactors = FALSE,
                         colClasses = c("numeric"))
  hiatus_tb <- read.csv(file.path("../hiatus.csv"),
                        header = TRUE,
                        stringsAsFactors = FALSE,
                        colClasses = c("numeric", "numeric"))
  core <- read.csv(paste0(entity, ".csv"),
                   header = TRUE,
                   stringsAsFactors = FALSE,
                   colClasses = c("character", "numeric", "numeric", "numeric"))

  accMean<- sapply(c(1, 2, 5), function(x) x * 10^(-1:2))
  ballpacc <- lm(core[,2] * 1.1 ~ core[, 4])$coefficients[2]
  ballpacc <- abs(accMean - ballpacc)
  ballpacc <- ballpacc[ballpacc > 0]
  accMean <- accMean[order(ballpacc)[1]]

  k <- seq(floor(min(depth_eval, na.rm = TRUE)),
           ceiling(max(depth_eval, na.rm = TRUE)),
           by = 5)
  if (k < 10) {
    thickness <- pretty(5 * (k/10), 10)
    thickness <- min(thickness[thickness > 0])
  } else if (k > 20) {
    thickness <- max(pretty(5 * (k/20)))
  }

  j <- 2000
  tho <- c()


  setwd(file.path(wdir, "Bacon_runs"))
  unknown_age <- read.csv("../not_used_dates.csv", header = TRUE)
  setwd(file.path(wdir))
  print("#------------ run bacon ---------------#")
  if (nrow(hiatus_tb) == 0) {
    tryCatch({
      rbacon::Bacon(core = entity,
                    depths.file = TRUE,
                    thick = thickness,
                    acc.mean = accMean,
                    postbomb = postbomb,
                    cc = cc,
                    suggest = FALSE,
                    ask = FALSE,
                    ssize = j,
                    th0 = tho)
    },
    error = function(e) {
      write.table(x = paste("ERROR in Bacon:", conditionMessage(e)),
                  file = "bacon_error.txt")
    })
  } else {
    tryCatch({
      rbacon::Bacon(core = entity,
                    depths.file = TRUE,
                    thick = thickness,
                    acc.mean = accMean ,
                    postbomb = postbomb,
                    hiatus.depths = hiatus_tb$depth_sample_bacon,
                    cc = cc,
                    suggest = F,
                    ask = F,
                    ssize = j,
                    th0 = tho)
    },
    error = function(e) {
      write.table(x = paste("ERROR in Bacon:", conditionMessage(e)),
                  file = "bacon_error.txt")
    })
  }

  print("--------------save data -----------------")
  bacon_mcmc <- sapply(depth_eval, rbacon::Bacon.Age.d)
  bacon_age <- get_bacon_median_quantile(depth_eval, hiatus_tb, bacon_mcmc)
  bacon_mcmc <- rbind(depth_eval, bacon_mcmc)
  bacon_mcmc <- t(bacon_mcmc)
  bacon_mcmc <- cbind(sample_id, bacon_mcmc)

  h <- cbind(hiatus_tb,
             matrix(NA,
                    nrow = dim(hiatus_tb)[1],
                    ncol = dim(bacon_mcmc)[2] - 2))
  names(h) <- names(bacon_mcmc)

  bacon_mcmc <- rbind(bacon_mcmc, h)
  bacon_mcmc <- bacon_mcmc[order(bacon_mcmc[,2]),]

  sample_id <- bacon_mcmc[,1]

  setwd(file.path(wdir, "Bacon_runs", entity))
  write.table(bacon_mcmc,
              "mc_bacon_ensemble.txt",
              col.names = FALSE,
              row.names = FALSE)
  write.csv(cbind(sample_id, bacon_age[ ,2:4]),
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
  points(x = core$corr_age,
    y = core$depth_dating_new * 10,
    lty = 2,
    col = "orange",
    pch = 4)
  arrows(core$corr_age - core$corr_age_uncert,
         core$depth_dating_new * 10,
         core$corr_age + core$corr_age_uncert,
         core$depth_dating_new * 10,
         length = 0.05,
         angle = 90,
         code = 3,
         col = "orange"
  )
  if (!plyr::empty(data.frame(hiatus_tb))) {
    abline(h = hiatus_tb$depth_sample_bacon * 10,
           col = "grey",
           lty = 2)
  }
  dev.off()
}

#' Determine median and quantiles for Bacon MC ensemble.
#'
#' @importFrom stats median
#' @importFrom stats quantile
#' @param depth_eval Sample depths for which interpolated age is wanted.
#' @param hiatus_tb Table containing the hiatus depths.
#' @param bacon_mcmc Bacon MC ensemble
#' @param q1 quantile 1
#' @param q2 quantile 2
#' @return Table containing the sample depths, median ages and uncertainties.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'     get_bacon_median_quantile(depth_tb, hiatus_depth, bacon_mcmc, 0.05, 0.95)
#' }
#'
#' @references
#' Comas-Bru, L. et al., SISALv2: A comprehensive speleothem isotope database
#' with multiple age-depth models, Earth Syst. Sci. Data Discuss (2019)
#' \url{https://github.com/paleovar/SISAL.AM}
get_bacon_median_quantile <- function(depth_eval,
                                      hiatus_tb,
                                      bacon_mcmc,
                                      q1 = 0.05,
                                      q2 = 0.95) {
  #bacon_mcmc <- sapply(depth_eval, Bacon.Age.d)
  bacon_age <- apply(bacon_mcmc, 2, median)
  bacon_quantile <- apply(bacon_mcmc,
                          2,
                          function(x) {
                            quantile(x, probs = c(q1, q2), na.rm = T)
                          })

  data <- cbind(depth_eval, bacon_age, bacon_age_uncert_pos = bacon_quantile[2,]-bacon_age,
                bacon_age_uncert_neg = bacon_age - bacon_quantile[1,])
  h <- data.frame(depth_eval = hiatus_tb$depth_sample_bacon,
                  bacon_age = replicate(dim(hiatus_tb)[1], NA),
                  bacon_age_uncert_pos = replicate(dim(hiatus_tb)[1], NA),
                  bacon_age_uncert_neg = replicate(dim(hiatus_tb)[1], NA))
  data <- rbind(data, h)
  data <- data[order(data[,1]),]

  return(data)
}
