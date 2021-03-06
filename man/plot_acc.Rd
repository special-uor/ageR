% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-plot.R
\name{plot_acc}
\alias{plot_acc}
\title{Plot accumulation rate}
\usage{
plot_acc(
  K,
  output,
  acc.mean = 20,
  acc.shape = 1.5,
  thick = 5,
  hiatuses = NULL,
  plot = TRUE,
  xlab = "Acc. rate [yr/cm]",
  ylab = NULL,
  title = NULL,
  ...
)
}
\arguments{
\item{K}{Number of sections in the core.}

\item{output}{Last MCMC output.}

\item{acc.mean}{The accumulation rate prior consists of a gamma distribution
with two parameters. Its mean is set by acc.mean (default
acc.mean=20 yr/cm (or whatever age or depth units are chosen), which can
be changed to, e.g., 5, 10 or 50 for different kinds of deposits).
Multiple values can be given in case of hiatuses or boundaries, e.g.,
Bacon(hiatus.depths=23, acc.mean=c(5,20)).}

\item{acc.shape}{The prior for the accumulation rate consists of a gamma
distribution with two parameters. Its shape is set by acc.shape
(default acc.shape=1.5; higher values result in more peaked shapes).}

\item{thick}{Bacon will divide the core into sections of equal thickness
specified by \code{thick} (default \code{thick = 5}).}

\item{hiatuses}{Data frame containing information of hiatuses.}

\item{plot}{Boolean flag to indicate whether a plot should be generated or
just return a data frame with posterior and prior values.}

\item{xlab}{\code{x}-axis label.}

\item{ylab}{\code{y}-axis label.}

\item{title}{Plot title.}

\item{...}{Optional parameters for
\code{\link[ggplot2:stat_function]{ggplot2::stat_function}}.}
}
\value{
List with \code{ggplot2} object and data frame with posterior and
    prior values.
}
\description{
Plot accumulation rate prior and posterior.
}
\examples{
\dontrun{
out <- read.table("Bacon_runs/core/core_K.out")
plot_acc(out$K, out$output)
}

}
\keyword{internal}
