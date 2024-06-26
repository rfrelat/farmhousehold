% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/farmhousehold.R
\name{select_farmhousehold}
\alias{select_farmhousehold}
\title{Subset the household}
\usage{
select_farmhousehold(x, idlist)
}
\arguments{
\item{x}{farmhousehold object}

\item{idlist}{vector with the hhid selected}
}
\description{
Subset the household
}
\examples{
data(hhdb_rhomis)
# select household with livestock
id <- sort(unique(hhdb_rhomis$lstk$hhid))
lstkfarm <- select_farmhousehold(hhdb_rhomis, id)
}
