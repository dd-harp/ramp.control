% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vector_control.R
\name{dynamic_vector_control.none}
\alias{dynamic_vector_control.none}
\title{Turn On Vector Control}
\usage{
\method{dynamic_vector_control}{none}(pars)
}
\arguments{
\item{pars}{an **`xds`** object}
}
\value{
an **`xds`** object
}
\description{
If
`class(pars$vector_control) == 'none'`
then dynamic vector_control has not
been set up by any other function.
This sets `class(vector_control) <- 'dynamic'` and
then sets up a  trivial module for every mode of
vector control.
}
