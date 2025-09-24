
#' @title Set Up the IRS effect_sizes Object
#'
#' @description Set up an object to model
#' the effect_sizes of IRS, usually through the
#' computation of some intermediate variable
#'
#' @param name the model name
#' @param xds_obj a **`ramp.xds`**  model object
#' @param s the vector species index
#' @param options a list of options to override defaults
#'
#' @return a **`ramp.xds`** model object
#' @export
setup_irs_effect_sizes = function(name, xds_obj, s=1, options=list()){
  class(name) <- name
  UseMethod("setup_irs_effect_sizes", name)
}

#' @title Set Up the Null IRS effect_sizes Object
#'
#' @description
#' Set up a model for IRS effect_sizes that does nothing.
#'
#' @inheritParams setup_irs_effect_sizes
#'
#' @return a **`ramp.xds`** model object
#'
#' @export
setup_irs_effect_sizes.none = function(name, xds_obj, s=1, options=list()){
  xds_obj$irs_obj$eff_sz_obj = make_none_object()
  return(xds_obj)
}

#' @title IRS effect_sizes
#'
#' @description Set the value of variables to model
#' the effect_sizes of IRS
#'
#' @param t current simulation time
#' @param y state variables
#' @param xds_obj a **`ramp.xds`** model object
#' @param s the vector species index
#'
#' @return a **`ramp.xds`** model object
#' @export
IRS_Effect_Sizes <- function(t, y, xds_obj, s=1) {
  UseMethod("IRS_Effect_Sizes", xds_obj$irs_obj$eff_sz_obj)
}

#' @title Set no use_bednets
#' @description The null model for use_bednets
#' @inheritParams IRS_Effect_Sizes
#' @return [list]
#' @export
IRS_Effect_Sizes.none <- function(t, y, xds_obj, s=1) {
  return(xds_obj)
}
