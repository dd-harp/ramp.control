
#' @title Set Up the Bed Net Ownership Object
#'
#' @description Set up an object to model
#' bed net ownership
#'
#' @param name the model name
#' @param xds_obj a **`ramp.xds`**  model object
#' @param options a list of options to override defaults
#'
#' @return a **`ramp.xds`** model object
#' @export
setup_bednet_own = function(name, xds_obj, options=list()){
  class(name) <- name
  UseMethod("setup_bednet_own", name)
}

#' @title Set Up the Null Bed Net Ownership Object
#'
#' @description
#' Set up the null bed net ownership object
#'
#' @param name the name of a model to set up
#' @param xds_obj a **`ramp.xds`**  model object
#' @param options a list of options to override defaults
#'
#' @return a **`ramp.xds`** model object
#'
#' @export
setup_bednet_own.none = function(name, xds_obj, options=list()){
  xds_obj$bednet_obj$own_obj = make_none_object()
  return(xds_obj)
}

#' @title Set the own_bednets
#' @description Set the value of exogenous variables related to
#' own_bednets
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
Own_Bed_Net <- function(t, xds_obj) {
  UseMethod("Own_Bed_Net", xds_obj$bednet_obj$own_obj)
}

#' @title Set no own_bednets
#' @description The null model for own_bednets
#' @inheritParams Own_Bed_Net
#' @return a **`ramp.xds`** model object
#' @export
Own_Bed_Net.none <- function(t, xds_obj) {
  return(xds_obj)
}

