# generic methods for mass distribution of drugs, vaccines, etc.

#' @title Methods for mass distributing health interventions
#' @description This method dispatches on the type of `xds_obj$mass_health`.
#' @param t current simulation time
#' @param y state variables
#' @param xds_obj a a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
MassHealth <- function(t, y, xds_obj) {
  UseMethod("MassHealth", xds_obj$mass_health)
}

#' @title Methods for distributing interventions during mass_healthal visits
#' @description Implements [MassHealth] for the none model (do nothing)
#' @inheritParams MassHealth
#' @return a **`ramp.xds`** model object
#' @export
MassHealth.none <- function(t, y, xds_obj) {
  return(xds_obj)
}

#' @title Set up the none model for mass_healthal distribution (do nothing)
#' @param xds_obj a a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
setup_no_mass_health  <- function(xds_obj) {
  mass_health <- list()
  class(mass_health) <- 'none'
  xds_obj$mass_health <- mass_health
  return(xds_obj)
}
