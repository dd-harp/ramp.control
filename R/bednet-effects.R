
#' @title Set Up the Bed Net Effects Object
#'
#' @description Set up an object to compute
#' an effect of bed nets, such as to modify
#' the availability of human hosts for blood
#' feeding
#'
#' @param name the model name
#' @param xds_obj an **`xds`**  model object
#' @param options a list of options to override defaults
#'
#' @return the **`xds`** model object
#' @export
setup_bednet_effects = function(name, xds_obj, options=list()){
  class(name) <- name
  UseMethod("setup_bednet_effects", name)
}

#' @title Set Up the Null Bed Net effectsership Object
#'
#' @description
#' Set up the null bed net effectsership object
#'
#' @param name the name of a model to set up
#' @param xds_obj an **`xds`**  model object
#' @param options a list of options to override defaults
#'
#' @return the **`xds`** model object
#'
#' @export
setup_bednet_effects.none = function(name, xds_obj, options=list()){
  xds_obj$bednet_obj$effects_obj = make_none_object()
  return(xds_obj)
}

#' @title Bed Net Effects
#'
#' @description Set the value of variables in a model that
#' implements the effects of bed nets
#'
#' @param t current time
#' @param xds_obj an **`xds`** model object
#'
#' @return the **`xds`** model object
#' @export
Bed_Net_Effects <- function(t, xds_obj) {
  UseMethod("Bed_Net_Effects", xds_obj$bednet_obj$effects_obj)
}

#' @title No Effects of Bed Nets
#'
#' @description Do nothing
#'
#' @inheritParams Bed_Net_Effects
#'
#' @return the **`xds`** model object
#'
#' @export
Bed_Net_Effects.none <- function(t, xds_obj) {
  return(xds_obj)
}

