% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tools.R
\name{conv_fun}
\alias{conv_fun}
\title{Find conversion factors for a vector of units}
\usage{
conv_fun(
  conv,
  x,
  other = NULL,
  cate = NULL,
  labother = c("other", "otherspecify", "other_specify"),
  warn = TRUE
)
}
\arguments{
\item{conv}{the conversion factors}

\item{x}{the original unit names}

\item{other}{the unit names of other (optional)}

\item{cate}{conversion categories if x are numbers instead of string}

\item{labother}{label for 'other'}

\item{warn}{whether to show a warning if missing names}
}
\value{
the vector with the conversion factors
}
\description{
Find conversion factors for a vector of units
}
