% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tie_rwalk.R
\name{tie_rwalk}
\alias{tie_rwalk}
\title{Tie random walk function}
\usage{
tie_rwalk(
  dat = NULL,
  RF = NULL,
  CF = NULL,
  id = NULL,
  RV = NULL,
  ord = NULL,
  prefLimit = 50,
  setseed = FALSE,
  compstudy = NULL,
  default = NULL,
  R = NULL
)
}
\arguments{
\item{dat}{imported (binarized) data frame}

\item{RF}{name of the reference fluid variable}

\item{CF}{name of the combination fluid variable}

\item{id}{subject IDs}

\item{RV}{name of the response variable}

\item{ord}{item category order}

\item{prefLimit}{preference limit for binarization threshold}

\item{setseed}{BOOLEAN; set a random seed TRUE/FALSE?}

\item{compstudy}{label of the compiled sub study (used for filtering)}

\item{default}{default item in worth value estimation (usually the lowest worth value)}

\item{R}{number of randomizations}
}
\value{
Exports random binarize response for distance cutoff selection
}
\description{
The \code{tie_rwalk} function prepares binary and continuous data for tiefightR analysis.
}
