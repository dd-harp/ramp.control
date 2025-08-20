
#' @title Set the treat_habitats_lsm
#' @description Set the value of exogenous variables related to
#' treat_habitats_lsm
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
TreatHabitats <- function(t, xds_obj) {
  UseMethod("TreatHabitats", xds_obj$lsm$treat_habitats)
}


#' @title Set up dynamic lsm
#' @description If dynamic lsm has not
#' already been set up, then turn on dynamic
#' lsm and set all the
#' @param name the name of a model to set up
#' @param xds_obj a **`ramp.xds`** model object
#' @param opts a list of options to override defaults
#' @return a **`ramp.xds`** model object
#' @export
setup_treat_habitats = function(name, xds_obj, opts=list()){
  class(name) <- name
  UseMethod("setup_treat_habitats", name)
}
