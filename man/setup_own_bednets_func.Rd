% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bednet-functions.R
\name{setup_own_bednets_func}
\alias{setup_own_bednets_func}
\title{Set up dynamic forcing}
\usage{
setup_own_bednets_func(
  pars,
  opts = list(),
  mean = 0.7,
  F_season = F_flat,
  F_trend = F_flat
)
}
\arguments{
\item{pars}{an **`xds`** object}

\item{opts}{a list of options to override defaults}

\item{mean}{mean bednet ownership}

\item{F_season}{the seasonal signal in bednet ownership}

\item{F_trend}{a temporal trend in bednet ownership}
}
\value{
an **`xds`** object
}
\description{
If dynamic forcing has not
already been set up, then turn on dynamic
forcing and set all the
}
