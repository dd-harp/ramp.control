
#' @title Set Up the IRS Effects Object
#'
#' @description Set up an object to model
#' the effects of IRS, usually through the
#' computation of some intermediate variable
#'
#' @param name the model name
#' @param xds_obj a **`ramp.xds`**  model object
#' @param options a list of options to override defaults
#'
#' @return a **`ramp.xds`** model object
#' @export
setup_irs_effects = function(name, xds_obj, options=list()){
  class(name) <- name
  UseMethod("setup_irs_effects", name)
}

#' @title Set Up the Null IRS Effects Object
#'
#' @description
#' Set up a model for IRS effects that does nothing.
#'
#' @param name the name of a model to set up
#' @param xds_obj a **`ramp.xds`**  model object
#' @param options a list of options to override defaults
#'
#' @return a **`ramp.xds`** model object
#'
#' @export
setup_irs_effects.none = function(name, xds_obj, options=list()){
  xds_obj$irs_obj$effects_obj = make_none_object()
  return(xds_obj)
}

#' @title IRS Effects
#'
#' @description Set the value of variables to model
#' the effects of IRS
#'
#' @param t current simulation time
#' @param y state variables
#' @param xds_obj a **`ramp.xds`** model object
#'
#' @return a **`ramp.xds`** model object
#' @export
IRS_Effects <- function(t, y, xds_obj) {
  UseMethod("IRS_Effects", xds_obj$irs_obj$effects_obj)
}

#' @title Set no use_bednets
#' @description The null model for use_bednets
#' @inheritParams IRS_Effects
#' @return [list]
#' @export
IRS_Effects.none <- function(t, y, xds_obj) {
  return(xds_obj)
}
