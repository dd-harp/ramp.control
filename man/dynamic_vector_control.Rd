% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vector_control.R
\name{dynamic_vector_control}
\alias{dynamic_vector_control}
\title{Turn On Vector Control}
\usage{
dynamic_vector_control(pars)
}
\arguments{
\item{pars}{an **`xds`** object}
}
\value{
an **`xds`** object
}
\description{
Any function that sets up
non-trivial vector control must
calls this function.
If `class(pars$vector_control) == 'none'`
then it is set to `dynamic` and the
trivial module for every mode of
vector control is set up. Otherwise,
nothing happens.
}
