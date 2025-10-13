
#' @title Set Up the House Spraying Object
#'
#' @description Set up an object to model
#' house spraying
#'
#' @param name the model name
#' @param xds_obj a **`ramp.xds`**  model object
#' @param options a list of options to override defaults
#'
#' @return a **`ramp.xds`** model object
#' @export
setup_spray_houses = function(name, xds_obj, options=list()){
  class(name) <- name
  UseMethod("setup_spray_houses", name)
}

#' @title Set Up the Null House Spraying Object
#'
#' @description
#' Set up the null house spraying model object
#'
#' @param name the name of a model to set up
#' @param xds_obj a **`ramp.xds`**  model object
#' @param options a list of options to override defaults
#'
#' @return a **`ramp.xds`** model object
#'
#' @export
setup_spray_houses.none = function(name, xds_obj, options=list()){
  xds_obj$irs_obj$spray_obj = make_none_object()
  return(xds_obj)
}

#' @title Spray Houses
#'
#' @description The model
#'
#' @param t current simulation time
#' @param y state variables
#' @param xds_obj a **`ramp.xds`** model object
#'
#' @return a **`ramp.xds`** model object
#'
#' @export
SprayHouses <- function(t, y, xds_obj) {
  UseMethod("SprayHouses", xds_obj$irs_obj$spray_obj)
}

#' @title No House Spraying
#'
#' @description The null model for house spraying
#'
#' @inheritParams SprayHouses
#'
#' @return a **`ramp.xds`** model object
#' @export
SprayHouses.none <- function(t, y, xds_obj) {
  return(xds_obj)
}
