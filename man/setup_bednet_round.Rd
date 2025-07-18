% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bednet-coverage-multiround.R
\name{setup_bednet_round}
\alias{setup_bednet_round}
\title{Set up dynamic forcing}
\usage{
setup_bednet_round(type, t_init, coverage, zap = 1)
}
\arguments{
\item{type}{the name of the BedNet type}

\item{t_init}{the time when BedNet started}

\item{coverage}{the coverage achieved}

\item{zap}{contact scaling}
}
\value{
an **`xds`** object
}
\description{
If dynamic forcing has not
already been set up, then turn on dynamic
forcing and set all the
}
