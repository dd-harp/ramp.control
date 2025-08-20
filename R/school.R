# generic methods for distributing interventions at schools

#' @title Methods for distributing interventions during schoolal visits
#' @description This method dispatches on the type of `xds_obj$school`.
#' @param t current simulation time
#' @param y state variables
#' @param xds_obj a a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
School <- function(t, y, xds_obj) {
  UseMethod("School", xds_obj$school)
}

#' @title Methods for distributing interventions during schoolal visits
#' @description Implements [School] for the none model (do nothing)
#' @inheritParams School
#' @return a **`ramp.xds`** model object
#' @export
School.none <- function(t, y, xds_obj) {
  return(xds_obj)
}

#' @title Set up the none model for schoolal distribution (do nothing)
#' @param xds_obj a a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
setup_no_school  <- function(xds_obj) {
  school <- list()
  class(school) <- 'none'
  xds_obj$school <- school
  return(xds_obj)
}
