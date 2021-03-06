% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-plot.R
\name{plot_grid}
\alias{plot_grid}
\title{Create grid of plots}
\usage{
plot_grid(
  plots,
  scenarios,
  cond_x = "x",
  cond_y = "y",
  cond_x_units = NULL,
  cond_y_units = NULL,
  append_title = FALSE,
  ...
)
}
\arguments{
\item{plots}{List with \code{ggplot2} objects.}

\item{scenarios}{Data frame with scenarios, must contain only 2 variables.}

\item{cond_x}{Condition on the \code{x}-axis.}

\item{cond_y}{Condition on the \code{y}-axis.}

\item{cond_x_units}{Units for condition on the \code{x}-axis.}

\item{cond_y_units}{Units for condition on the \code{y}-axis.}

\item{...}{Optional parameters for
\code{\link[gridExtra:grid.arrange]{gridExtra::grid.arrange}}.}
}
\value{
List of gridded \code{ggplot2} objects.
}
\description{
Create grid of \code{ggplot2} objects, based on a data frame with scenarios.
}
\keyword{internal}
