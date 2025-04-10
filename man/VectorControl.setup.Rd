% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vector_control.R
\name{VectorControl.setup}
\alias{VectorControl.setup}
\title{Vector Control for Static Vector Control}
\usage{
\method{VectorControl}{setup}(t, y, pars)
}
\arguments{
\item{t}{current simulation time}

\item{y}{state vector}

\item{pars}{an \strong{\code{xds}} object}
}
\value{
an **`xds`** object
}
\description{
The `setup` case runs the `dynamic`
case once, to set the values of variables for
static models. It then reverts to `none` so that
those values are not changed again.
}
