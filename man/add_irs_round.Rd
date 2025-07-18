% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/irs-coverage-multiround.R
\name{add_irs_round}
\alias{add_irs_round}
\title{Set up dynamic forcing}
\usage{
add_irs_round(pars, type, t_init, coverage, zap = 1)
}
\arguments{
\item{pars}{the `ramp.xds` object}

\item{type}{the name of the IRS type}

\item{t_init}{the time when IRS started}

\item{coverage}{the coverage achieved}

\item{zap}{the coverage achieved}
}
\value{
an **`xds`** object
}
\description{
If dynamic forcing has not
already been set up, then turn on dynamic
forcing and set all the
}
