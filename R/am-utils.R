#' Add artificial hiatus
#'
#' Add artificial hiatus date to the input dating files for StalAge, Bchron and
#'     lin. Interp.
#' @importFrom dplyr `%>%`
#' @importFrom stats approx
#' @importFrom stats lag
#' @param data Input dating information.
#' @param hiatus_tb Table containing the sample_id and depth of each hiatus.
#' @param stalage Logical. TRUE if the artificial hiatus ages is to be added
#'     to the Stalage input file.
#' @param bchron Logical. TRUE if the artificial hiatus ages is to be added
#'     to the Bchron input file.
#' @param linInterp Logical. TRUE if the artificial hiatus ages is to be added
#'     to the lin. Interp. input file.
#' @return The modified date file.
# @example add_hiatus(dating_tb, hiatus, bchron = TRUE)
#'
#' @references
#' Comas-Bru, L. et al., SISALv2: A comprehensive speleothem isotope database
#' with multiple age-depth models, Earth Syst. Sci. Data Discuss (2019)
#' \url{https://github.com/paleovar/SISAL.AM}
add_hiatus <- function(data,
                       hiatus_tb,
                       stalage = FALSE,
                       bchron = FALSE,
                       linInterp = FALSE) {
  age <- unlist(data[, 2])
  depth_dating <- unlist(data[, 4])
  x_out <- unlist(hiatus_tb$depth_sample)

  e <- approx(x = depth_dating,
              y = age,
              xout = x_out,
              method = "linear")

  if (linInterp) {
    e <- approx(x = depth_dating,
                y = age,
                xout = x_out,
                method = "linear")
    h <- data.frame(dating_id = hiatus_tb$sample_id,
                    corr_age = e$y,
                    corr_age_uncert = rep(NA, length(hiatus_tb$depth_sample)),
                    depth_dating = e$x,
                    date_type = rep("Hiatus", length(hiatus_tb$depth_sample)))
    new <- rbind(data, h)
    new <-new %>%
      dplyr::arrange(., corr_age) %>%
      dplyr::mutate(corr_age_uncert =
                      dplyr::if_else(is.na(corr_age_uncert),
                                     ((lead(corr_age_uncert) +
                                         lead(corr_age) - corr_age) +
                                        (corr_age - (lag(corr_age) -
                                                       lag(corr_age_uncert)))
                                      ) / 2,
                                     as.double(corr_age_uncert)))
  }

  if (stalage) {
    e <- approx(x = depth_dating,
                y = age,
                xout = x_out,
                method = "linear")
    h <- data.frame(dating_id = hiatus_tb$sample_id,
                    corr_age = e$y,
                    corr_age_uncert = rep(NA, length(hiatus_tb$depth_sample)),
                    depth_dating = e$x)
    new <- rbind(data, h)
    new <- new %>%
      dplyr::arrange(., corr_age) %>%
      dplyr::mutate(corr_age_uncert =
                      dplyr::if_else(is.na(corr_age_uncert),
                                     ((lead(corr_age_uncert) +
                                         lead(corr_age) - corr_age) +
                                        (corr_age - (lag(corr_age) -
                                                       lag(corr_age_uncert)))
                                      ) / 2,
                                     as.double(corr_age_uncert)))
  }

  if(bchron) {
    x_out <- x_out / 10
    e <- approx(x = depth_dating,
                y = age,
                xout = x_out,
                method = "linear")

    h <- data.frame(dating_id = hiatus_tb$sample_id,
                    corr_age = e$y,
                    corr_age_uncert = rep(NA, length(hiatus_tb$depth_sample)),
                    depth_dating_new = e$x,
                    thickness_new = rep(NA, length(hiatus_tb$depth_sample)),
                    calib_curve_new = "normal")

    new <- rbind(data, h)
    new <- new %>%
      dplyr::arrange(., corr_age) %>%
      dplyr::mutate(corr_age_uncert =
                      dplyr::if_else(is.na(corr_age_uncert),
                                     ((lead(corr_age_uncert) +
                                         lead(corr_age) - corr_age) +
                                        (corr_age - (lag(corr_age) -
                                                       lag(corr_age_uncert)))
                                      ) / 2,
                                     as.double(corr_age_uncert)),
                    thickness_new =
                      dplyr::if_else(is.na(thickness_new),
                                     0.01,
                                     as.double(thickness_new)),
                    calib_curve_new =
                      dplyr::if_else(is.na(calib_curve_new),
                                     "normal",
                                     calib_curve_new))
  }
  return(new)
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
# @examples
# \dontrun{
#     get_bacon_median_quantile(depth_tb, hiatus_depth, bacon_mcmc, 0.05, 0.95)
# }
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
                            quantile(x, probs = c(q1, q2), na.rm = TRUE)
                          })

  data <- cbind(depth_eval,
                bacon_age,
                bacon_age_uncert_pos = bacon_quantile[2, ] - bacon_age,
                bacon_age_uncert_neg = bacon_age - bacon_quantile[1, ])
  h <- data.frame(depth_eval = hiatus_tb$depth_sample_bacon,
                  bacon_age = replicate(dim(hiatus_tb)[1], NA),
                  bacon_age_uncert_pos = replicate(dim(hiatus_tb)[1], NA),
                  bacon_age_uncert_neg = replicate(dim(hiatus_tb)[1], NA))
  data <- rbind(data, h)
  data <- data[order(data[,1]),]

  return(data)
}

#' Linear interpolation for single MC simulation.
#' @importFrom dplyr `%>%`
#' @param data Table containing single age ensemble and dating depths
#' @param depth_eval Sample depths.
#' @param hiatus_tb Table conatining hiatus depths and sample_id's
#' @return Interpolated ages for sample depths.
#'
#' @references
#' Comas-Bru, L. et al., SISALv2: A comprehensive speleothem isotope database
#' with multiple age-depth models, Earth Syst. Sci. Data Discuss (2019)
#' \url{https://github.com/paleovar/SISAL.AM}
get_lin_interp <- function(data, depth_eval, hiatus_tb) {
  age <- unlist(data[, 2])
  depth_dating <- unlist(data[, 1])
  x_out <- unlist(depth_eval)

  e <- approxExtrap(x = depth_dating, y = age, xout = x_out)
  linInterp <- dplyr::as_tibble(data.frame(depth_eval = unlist(e$x),
                                           lin_interp_age = unlist(e$y)))

  if (!plyr::empty(data.frame(hiatus_tb))) {
    linInterp <- linInterp %>%
      dplyr::rowwise() %>%
      dplyr::mutate(lin_interp_age = dplyr::if_else(depth_eval %in% hiatus_tb,
                                                    NA_real_,
                                                    lin_interp_age))
  }
  return(linInterp)
}

#' Determine median and quantiles for MC ensemble; not Bacon.
#'
#' @param upd Table of Mc ensemble.
#' @param q1 quantile 1
#' @param q2 quantile 2
#' @return Table containing the sample depths, median ages and uncertainties.
#'
# @example get_median_quantile(mcmc, 0.05, 0.95)
#'
#' @references
#' Comas-Bru, L. et al., SISALv2: A comprehensive speleothem isotope database
#' with multiple age-depth models, Earth Syst. Sci. Data Discuss (2019)
#' \url{https://github.com/paleovar/SISAL.AM}
get_median_quantiles <- function(upd, q1, q2) {
  age_median <- apply(upd, 1, median)
  age_sd <- apply(upd,
                  1,
                  function(x) {
                    quantile(x, probs = c(q1, q2), na.rm = TRUE)
                  })
  return(cbind(age_median, t(age_sd)))
}

#' Linear regression slopes and intecepts for single MC simulation
#'
#' @param data Table containing single age ensemble and dating depths
#' @param hiatus_tb Table conatining hiatus depths and sample_id's
#' @return Table containing slopes and interceptions for each section.
#'
#' @references
#' Comas-Bru, L. et al., SISALv2: A comprehensive speleothem isotope database
#' with multiple age-depth models, Earth Syst. Sci. Data Discuss (2019)
#' \url{https://github.com/paleovar/SISAL.AM}
linear_regression <- function(data, hiatus_tb) { # data = c("depth","age")
  # initialize
  j <- length(hiatus_tb)
  m <- list(list())

  # calculate slope and intercept for each section between hiati
  idx <- data[, 1] < hiatus_tb[1]
  m[[1]] <- lm(data[idx, 2] ~ data[idx, 1])
  if(is.null(dim(data[idx, ]))) {
    m[[1]] <- NA
  }

  if (j >= 2) {
    for (i in seq(from = 2, to = j)) {
      idx <- (data[, 1] < hiatus_tb[i]) & (data[, 1] > hiatus_tb[i - 1])
      m[[i]] <- lm(data[idx, 2] ~ data[idx, 1])
      if (is.null(dim(data[idx, ]))) {
        m[[i]] <- NA
      }
    }

    idx <- data[, 1] > hiatus_tb[length(hiatus_tb)]
    m[[j + 1]] <- lm(data[idx, 2] ~ data[idx, 1])
    if (is.null(dim(data[idx, ]))) {
      m[[j + 1]] <- NA
    }

  } else {
    idx <- data[, 1] > hiatus_tb[length(hiatus_tb)]
    m[[j + 1]] <- lm(data[idx, 2] ~ data[idx, 1])
    if (is.null(dim(data[idx, ]))) {
      m[[j + 1]] <- NA
    }
  }
  return(m)
}

#' Interpolate ages using lin. reg. for single MC simulation; with hiatus.
#'
#' @param m Table containing slopes and interceptions for each section.
#' @param depth_eval Sample depths.
#' @param hiatus_tb Table containing hiatus depths and sample_id's.
#' @return Lin. regression fitted ages for sample depths.
#'
#' @references
#' Comas-Bru, L. et al., SISALv2: A comprehensive speleothem isotope database
#' with multiple age-depth models, Earth Syst. Sci. Data Discuss (2019)
#' \url{https://github.com/paleovar/SISAL.AM}
lin_reg_ages <- function(m, depth_eval, hiatus_tb) {
  # initialize
  d <- length(unlist(depth_eval))
  j <- length(m)
  lin_reg_age <- replicate(d, 0)

  for (i in seq(1, j)) {
    if (!is.na(m[[i]])) {
      for (k in c(1, 2)) {
        if (is.na(m[[i]]$coefficients[[k]])) {
          m[[i]]$coefficients[[k]] = 0
        }
      }
    }
  }

  # add column with ages to depth table
  depth <- cbind(depth_eval, lin_reg_age)

  idx <- depth[, 1] < hiatus_tb[1]
  if (is.na(m[[1]])) {
    depth[idx, 2] <- NA
  } else {
    depth[idx, 2] <- m[[1]]$coefficients[[1]] +
      depth[idx, 1] * m[[1]]$coefficients[[2]]
  }

  if (j>2) {
    for (i in seq(2, length(hiatus_tb))) {
      idx <- (depth[, 1] < hiatus_tb[i]) & (depth[, 1] > hiatus_tb[i - 1])
      if (is.na(m[[i]])) {
        depth[idx, 2] <- NA
      } else {
        depth[idx, 2] <- m[[i]]$coefficients[[1]] +
          depth[idx, 1] * m[[i]]$coefficients[[2]]
      }
    }

    idx <- depth[, 1] > hiatus_tb[length(hiatus_tb)]
    if (is.na(m[[1]])) {
      depth[idx, 2] <- NA
    } else {
      depth[idx, 2] <- m[[length(hiatus_tb) + 1]]$coefficients[[1]] +
        depth[idx, 1] * m[[length(hiatus_tb) + 1]]$coefficients[[2]]
    }
  } else {
    idx <- depth[, 1] > hiatus_tb[length(hiatus_tb)]
    if (is.na(m[[1]])) {
      depth[idx, 2] <- NA
    } else {
      depth[idx, 2] <- m[[length(hiatus_tb) + 1]]$coefficients[[1]] +
        depth[idx, 1] * m[[length(hiatus_tb) + 1]]$coefficients[[2]]
    }
  }
  data <- data.frame(depth_eval = depth[, 1], lin_reg_age = depth[, 2])
  h <- data.frame(depth_eval = hiatus_tb,
                  lin_reg_age = replicate(length(hiatus_tb), NA))
  data <- rbind(data, h)
  data <- data[order(data[, 1]), ]

  return(data)
}

#' Interpolate ages using lin. reg. for single MC simulation; no hiatus.
#'
#' @param data Table containing dates and dating depths.
#' @param depth_eval Sample depths.
#' @return Lin. regression fitted ages for sample depths.
#'
#' @references
#' Comas-Bru, L. et al., SISALv2: A comprehensive speleothem isotope database
#' with multiple age-depth models, Earth Syst. Sci. Data Discuss (2019)
#' \url{https://github.com/paleovar/SISAL.AM}
lin_reg_no_hiatus <- function(data, depth_eval) {
  m_lR<- lm(data[,2]~data[,1])

  age_lR <- replicate(length(unlist(depth_eval)), 0)
  depth <- cbind(depth_eval, age_lR)
  depth[,2]<- m_lR[1]$coefficients[[1]] + depth[,1]*m_lR[1]$coefficients[[2]]
  return(depth)

}

#' Generate age ensembles depending on AM method.
#'
#' @param linReg Logical. TRUE if MC ensemble is to be generated for lin. reg.
#' @param linInterp Logival. TRUE if MC ensemble is to be generated for lin. interp.
#' @param age Dates.
#' @param age_error Uncertainties to input dates.
#' @param N Number of interations.
#' @param wdir path where input files are stored.
#' @param entity name of the entity.
#'
#' @return Lin. reg. ensemble of N interations for sample depths.
#'
#' @references
#' Comas-Bru, L. et al., SISALv2: A comprehensive speleothem isotope database
#' with multiple age-depth models, Earth Syst. Sci. Data Discuss (2019)
#' \url{https://github.com/paleovar/SISAL.AM}
mc_ensemble <- function(linReg = FALSE,
                        linInterp = FALSE,
                        age,
                        age_error,
                        N,
                        wdir,
                        entity) {
  # N number of MC simulations
  number <- 0
  d <- 0
  age_ensemble_final <- NA

  if (linReg) {
    age_ensemble <- apply(cbind(age, age_error),
                          1,
                          function(x) {
                            rnorm(n = N, mean = x[1], sd = x[2])
                          }) # calculate N deviates for each age
    # age_ensemble_final <- age_ensemble
    # return(age_ensemble_final)
    return(age_ensemble)
  }

  if (linInterp) {
    while (d < N) {
      k <- N - d
      number <- number + 1
      age_ensemble <- apply(cbind(age, age_error),
                            1,
                            function(x) {
                              rnorm(n = k, mean = x[1], sd = x[2])
                            }) # calculate N deviates for each age

      run <- TRUE
      if (k == 1) {
        age_ensemble_diff <- diff(age_ensemble)
        if (any(age_ensemble_diff < 0 & !is.na(age_ensemble_diff))) {
          run <- FALSE
        }
      } else {
        # each column contains the derivatives for one MC run -> N columns
        age_ensemble_diff <- apply(age_ensemble, 1, diff)
        del <- NULL
        for (i in seq(1, k)) {
          if (any(age_ensemble_diff[,i] < 0)) {
            if (is.null(dim(age_ensemble)[1])) {
              run <- FALSE
            } else {
              del <- c(del, i)
            }
          }
        }
        if (!is.null(del)) {
          age_ensemble <- age_ensemble[-del, ]
        }
      }

      if (run) {
        if (k == N){
          age_ensemble_final <- age_ensemble
        } else {
          age_ensemble_final <- rbind(age_ensemble_final, age_ensemble)
        }
      }

      d <- dim(age_ensemble_final)[1]
      #diff_final <- apply(age_ensemble_final, 1, diff)
      #print(any(diff_final<0))
      if (is.null(d)) {
        d <- 1
      }
      if (number > 2000 && d > 100) {
        return(age_ensemble_final)
        break
      } else if(number > 2000 && d < 100) {
        setwd(file.path(wdir, entity, '/linInterp'))
        write.csv(c(number, d), 'mc_fail.csv', row.names = FALSE)
        stop('ERROR: too many iterations, check data!')
      }
    }
    return(age_ensemble_final) # ensemble
  }
}

#' Generate lin. interp. ensemble.
#'
#' @param N Number of interations.
#' @param hiatus_tb Table containing hiatus depths and hiatus sample_ids.
#' @param depth_dating Dating depths.
#' @param age_ensemble Mc ensemble of dating table.
#' @param depth_sample Sample depths.
#' @return Lin. interp. ensemble of N interations for sample depths.
#'
#' @references
#' Comas-Bru, L. et al., SISALv2: A comprehensive speleothem isotope database
#' with multiple age-depth models, Earth Syst. Sci. Data Discuss (2019)
#' \url{https://github.com/paleovar/SISAL.AM}
mc_linInt <- function(N, hiatus_tb, depth_dating, age_ensemble, depth_sample){
  for (j in 1:N) {
    if (j == 1) {
      age_mc <- get_lin_interp(cbind(depth_dating, age_ensemble[j, ]),
                               depth_sample,hiatus_tb)
      sample_ensemble <- age_mc
    } else {
      age_mc <- get_lin_interp(cbind(depth_dating, age_ensemble[j, ]),
                               depth_sample,hiatus_tb)
      sample_ensemble <- cbind(sample_ensemble, age_mc[, 2])
    }
  }
  return(sample_ensemble)
}

#' Generate lin. reg. ensemble.
#'
#' @param N Number of interations.
#' @param hiatus_tb Table containing hiatus depths and hiatus sample_ids.
#' @param depth_dating Dating depths.
#' @param age_ensemble Mc ensemble of dating table.
#' @param depth_sample Sample depths.
#' @return Lin. reg. ensemble of N interations for sample depths.
#'
#' @references
#' Comas-Bru, L. et al., SISALv2: A comprehensive speleothem isotope database
#' with multiple age-depth models, Earth Syst. Sci. Data Discuss (2019)
#' \url{https://github.com/paleovar/SISAL.AM}
mc_linReg <- function(N, hiatus_tb, depth_dating, age_ensemble, depth_sample) {
  for (i in 1:N) {
    if (i == 1) {
      if (plyr::empty(data.frame(hiatus_tb))) {
        age_mc <- lin_reg_no_hiatus(cbind(depth_dating,
                                          age_ensemble[i, ]),
                                    depth_sample)
      } else {
        m <- linear_regression(cbind(depth_dating, age_ensemble[i, ]),
                               hiatus_tb)
        age_mc <- linear_regression_ages(m, depth_sample, hiatus_tb)
      }
      sample_ensemble <- age_mc
    } else {
      if (plyr::empty(data.frame(hiatus_tb))) {
        age_mc <- lin_reg_no_hiatus(cbind(depth_dating,
                                          age_ensemble[i, ]),
                                    depth_sample)
      } else {
        m <- linear_regression(cbind(depth_dating, age_ensemble[i, ]),
                               hiatus_tb)
        age_mc <- linear_regression_ages(m, depth_sample, hiatus_tb)
      }
      sample_ensemble <- cbind(sample_ensemble, age_mc[, 2])
    }
  }
  return(sample_ensemble)
}
