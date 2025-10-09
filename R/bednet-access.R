
#' @title Set Up the Bed Net Access Object
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
setup_bednet_access = function(name, xds_obj, options=list()){
  class(name) <- name
  UseMethod("setup_bednet_access", name)
}

#' @title Set Up the Null Bed Net Access Object
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
setup_bednet_access.none = function(name, xds_obj, options=list()){
  xds_obj$bednet_obj$access_obj = make_none_object()
  return(xds_obj)
}

#' @title Compute Bed Net Access
#'
#' @description Set the value of bed net access
#'
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`** model object
#'
#' @return a **`ramp.xds`** model object
#'
#' @export
Bed_Net_Access <- function(t, xds_obj) {
  UseMethod("Bed_Net_Access", xds_obj$bednet_obj$access_obj)
}

#' @title Set no own_bednets
#' @description The null model for own_bednets
#' @inheritParams Bed_Net_Access
#' @return a **`ramp.xds`** model object
#' @export
Bed_Net_Access.none <- function(t, xds_obj) {
  return(xds_obj)
}

