# generic methods for distributing interventions at clinics

#' @title Methods for distributing interventions during clinical visits
#' @description This method dispatches on the type of `xds_obj$clinic`.
#' @param t current simulation time
#' @param y state variables
#' @param xds_obj a a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
Clinic <- function(t, y, xds_obj) {
  UseMethod("Clinic", xds_obj$clinic)
}

#' @title Methods for distributing interventions during clinical visits
#' @description Implements [Clinic] for the none model (do nothing)
#' @inheritParams Clinic
#' @return a **`ramp.xds`** model object
#' @export
Clinic.none <- function(t, y, xds_obj) {
  return(xds_obj)
}

#' @title Set up the none model for clinical distribution (do nothing)
#' @param xds_obj a a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
setup_no_clinic  <- function(xds_obj) {
  clinic <- list()
  class(clinic) <- 'none'
  xds_obj$clinic <- clinic
  return(xds_obj)
}
