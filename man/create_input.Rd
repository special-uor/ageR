% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-file.R
\name{create_input}
\alias{create_input}
\title{Create input for age model}
\usage{
create_input(data, wdir, entity = NULL, am = "bacon")
}
\arguments{
\item{data}{List with named data frames containing the input data.
\itemize{
  \item \code{sample_depths}: data frame with sampling depths information.
    \itemize{
      \item \code{id}: depth ID.
      \item \code{depth}: depth in cm.
    }
  \item \code{core}: data frame with core (entity) information.
    \itemize{
      \item \code{labID}: lab ID.
      \item \code{age}: age in years.
      \item \code{error}: age uncertainty in years.
      \item \code{depth}: depth in cm.
      \item (Optional) \code{cc}: calibration curve.
        \itemize{
          \item \code{cc = 1}: \code{IntCal20}
          (northern hemisphere terrestrial).
          \item \code{cc = 2}: \code{Marine20} (marine).
          \item \code{cc = 3}: \code{SHCal20}
          (southern hemisphere terrestrial).
        }
    }
 \item (Optional) \code{hiatus}: data frame with information of hiatus
 depths, if not passed, then an empty file is created.
    \itemize{
      \item \code{id}: depth ID.
      \item \code{depth}: depth in cm.
    }
}}

\item{wdir}{Working directory, absolute or relative path.}

\item{entity}{Entity name.}

\item{am}{Age model name.}
}
\value{
Input files for the age model.
}
\description{
Create input for age models, based on the file structure and input files
organisation needed.
}
\details{
If only \code{wdir} is passed, the value of \code{entity} will be extracted
from the last portion of \code{wdir}. For example, if
\code{wdir = "/path/to/a"}, then \code{entity = "a"} and
\code{wdir = "/path/to"}. If \code{entity = NULL} and \code{wdir = "/"}
the function will return an error message, because the path is too short.
To use the current working directory, \code{wdir = "./"}.
}
\examples{
test_data <-list(sample_depths = data.frame(id = 1:100,
                                            depth =
                                              seq(0, 500, length = 100)),
                 core = data.frame(labID = "RDG",
                                   age = c(720, 2700, 4660),
                                   error = c(70, 50, 110),
                                   depth = c(83, 212, 418)))
\dontrun{
ageR::create_input(test_data, getwd(), "Reading")
}
}
