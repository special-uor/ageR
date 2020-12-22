
#' @keywords internal
#' @references
#' Blaauw, M. and Christen, J.A., Flexible paleoclimate age-depth models using an autoregressive
#' gamma process. Bayesian Analysis 6 (2011), no. 3, 457--474.
#' \url{https://projecteuclid.org/euclid.ba/1339616472}
plot_age_depth2 <- function(ages,
                            set = get("info"),
                            depths = set$depths,
                            BCAD = set$BCAD,
                            depth.unit = "cm",
                            age.unit = "yr",
                            depths.file = FALSE,
                            hiatus.depths = NA,
                            kcal = FALSE,
                            d.min = set$d.min,
                            d.max = set$d.max,
                            d.by = set$d.by,
                            d.res = 400,
                            age.max = c(),
                            age.min = c(),
                            age.res = 40,
                            maxcalc = 500,
                            prob = set$prob,
                            rev.age = FALSE,
                            rev.d = FALSE,
                            d.lab = c(),
                            age.lab = c(),
                            thick = 5,
                            after = 1e-04 / thick,
                            slump = c(),
                            dark = c(),
                            rotate.axes = FALSE) {
  # calculate and plot the ranges and 'best' estimates for each required depth
  if(length(d.min) == 0)
    d.min <- set$d.min
  if(length(d.max) == 0)
    d.max <- set$d.max
  if(length(d.by) == 0)
    d.by <- set$d.by
  if(depths.file) {
    dfile <- paste0(set$coredir, set$core, "/", set$core, "_depths.txt")
    if(!file.exists(dfile))
      stop("I cannot find the file ", paste0(set$coredir, set$core, "/", set$core, "_depths.txt"), call.=FALSE)
    depths <- read.table(dfile, header=FALSE)[,1]
    if(!is.numeric(depths[1]))
      stop("File should contain numbers only, no headers", call.=FALSE)
  }
  if(length(depths) > 0)
    d <- sort(depths) else
      d <- seq(set$d.min, set$d.max, by=d.by) # not d.min itself as depths < set$d.min cannot be calculated. Same for d.max, best not extrapolate here
    if(length(d) > maxcalc)
      message("Warning, this will take quite some time to calculate. I suggest increasing d.by to, e.g.", 10*d.by, "\n") # was set$d.by

    # cosmetic; to avoid very steep plotted curves of mean and ranges across a hiatus
    for(i in set$hiatus.depths)
      d <- sort(unique(c(i + after, i, d)))
    for(i in set$slump)
      d <- sort(unique(c(i + after, i, d)))

    modelranges <- c()
    ranges <- rbacon:::Bacon.rng(d, set, BCAD = BCAD, prob = prob)

    # calculate calendar axis limits
    modelranges <- range(ranges[!is.na(ranges)])
    dates <- set$calib$probs
    dateranges <- c()
    for(i in 1:length(dates))
      if(BCAD)
        dateranges <- range(dateranges, 1950 - dates[[i]][,1]) else
          dateranges <- range(dateranges, dates[[i]][,1])
    if(length(age.min) == 0)
      age.min <- min(modelranges, dateranges)
    if(length(age.max) == 0)
      age.max <- max(modelranges, dateranges)
    # age.min <- min(ages)
    # age.max <- max(ages)
    age.lim <- extendrange(c(age.min, age.max), f = 0.01)

    if(BCAD)
      age.lim <- rev(age.lim)
    if(rev.age)
      age.lim <- rev(age.lim)
    d.lim <- rev(extendrange(c(d.max, d.min), f = 0.01))
    if(rev.d)
      d.lim <- d.lim[2:1]

    if (length(d.lab) == 0)
      d.lab <- paste0("Depth (", depth.unit, ")")
    if (length(age.lab) == 0)
      age.lab <- ifelse(BCAD, "BC/AD", ifelse(kcal, "kcal BP", paste("cal", age.unit, "BP")))

    if (kcal)
      ifelse(rotate.axes, xaxt <- "n", yaxt <- "n")
    oldpar <- par(mar = c(3, 3, 1, 1)) # no need for righthand axis
    on.exit(par(oldpar))

    # Base plot
    if (rotate.axes) {
      outp <- ggplot2::qplot() +
        ggplot2::geom_point() +
        ggplot2::xlim(age.lim) +
        ggplot2::ylim(d.lim) +
        ggplot2::labs(x = age.lab, y = d.lab)

      # if (kcal) {
      #   outp <- outp + ggplot2::xlim(pretty(age.lim))
      # } else {
      #   outp <- outp + ggplot2::xlim(age.lim)
      # }
      # outp <- outp + ggplot2::ylim(d.lim) +
      #   ggplot2::labs(x = age.lab, y = d.lab)
    } else {
      outp <- ggplot2::qplot() +
        ggplot2::geom_point() +
        ggplot2::xlim(d.lim[2:1]) + ggplot2::ylim(age.lim) +
        ggplot2::labs(x = d.lab, y = age.lab)
    }
    # if(kcal)
    #   axis(ifelse(rotate.axes, 1, 2), pretty(age.lim), pretty(age.lim/1e3))

    ### Plot
    print(d.min)
    print(d.max)
    print(d.res)
    dseq <- seq(d.min, d.max, length = d.res)
    rbacon::Bacon.hist(dseq, set, BCAD = set$BCAD, calc.range = FALSE, draw = FALSE)
    hists <- get('hists')
    scales <- array(0, dim = c(length(dseq), age.res))
    ageseq <- seq(min(age.lim), max(age.lim), length = age.res)
    for(i in 1:length(dseq)) {
      ages <- seq(hists[[i]]$th0, hists[[i]]$th1, length=hists[[i]]$n)
      if(length(!is.na(ages)) > 0)
        scales[i,] <- approx(ages, hists[[i]]$counts, ageseq, rule=2)$y
    }
    minmax <- hists[[length(hists)]]$min
    maxmax <- hists[[length(hists)]]$max
    scales <- scales/maxmax # normalise to the height of most precise age estimate
    if(length(dark) == 0)
      dark <- 10 * minmax / maxmax
    scales[scales > dark] <- dark
    dseq <- sort(dseq)

    df <- tibble::tibble(x = rep(ageseq, each = length(dseq)),
                         y = rep(dseq, length(ageseq)),
                         z = matrix(scales, byrow = TRUE))
    # image(ageseq, dseq, t(scale), col = grey(seq(1,0, length = 100)))
    greys <- RColorBrewer::brewer.pal(9, "Greys")
    if (rotate.axes) {
      df$tmp <- df$x
      df$x <- df$y
      df$y <- df$x
    }
    # ggplot2::ggplot(df, ggplot2::aes(x, y, z = z)) +
    outp <- outp +
      ggplot2::geom_contour_filled(data = df,
                                   mapping = ggplot2::aes(x, y, z = z)) +
      ggplot2::scale_fill_manual(values = c(NA,
                                            colorRampPalette(greys)(12)[-1])) +
      ggplot2::theme_bw()

    print(outp)
    # if(length(set$slump) > 0 )
    #   for(i in seq_len(nrow(set$slump)))
    #     if(rotate.axes)
    #       rect(min(age.lim)-1e3, set$slump[i,1], max(age.lim)+1e3, set$slump[i,2], col=slump.col, border=slump.col) else
    #         rect(set$slump[i,1], min(age.lim)-1e3, set$slump[i,2], max(age.lim)+1e3, col=slump.col, border=slump.col)

    # calib.plot(set, BCAD=BCAD, cc=cc, rotate.axes=rotate.axes, height=height, calheight=calheight, mirror=mirror, up=up, date.res=date.res, cutoff=cutoff, C14.col=C14.col, C14.border=C14.border, cal.col=cal.col, cal.border=cal.border, dates.col=dates.col, new.plot=FALSE, normalise.dists=normalise.dists, same.heights=same.heights)
    list(age = ageseq,
         depth = dseq,
         scale = scales)
    # if(rotate.axes)
    #   image(ageseq, dseq, t(scales), add=TRUE, col=colours, useRaster=FALS§E) else
    #     image(dseq, ageseq, scales, add=TRUE, col=colours, useRaster=FALSE)
}
plot_age_depth2(core$age)
