
#' @title Set Up the Null IRS Model Object
#'
#' @description
#' Sets up the null IRS model object
#'
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @return a **`ramp.xds`** model object
#'
#' @export
setup_irs_object = function(xds_obj){
  xds_obj$irs_obj = make_none_object()
  xds_obj <- setup_spray_houses("none", xds_obj, list())
  xds_obj <- setup_irs_effects("none", xds_obj,  list())
  xds_obj <- setup_irs_coverage("none", xds_obj, list())
  xds_obj$irs_obj$eff_sz_obj <- list()
  xds_obj <- setup_irs_effect_sizes("none", xds_obj, 1, list())
  return(xds_obj)
}

#' @title Set Up a Bed Net Model
#'
#' @description This sets up a bed net
#' model
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param spray_houses_name the name of a model for bed net ownership
#' @param spray_houses_opts options for the bed net ownership model
#' @param effects_name the name of a model for bed net effects
#' @param effects_opts options for the bed net effects model
#' @param coverage_name the name of a model for bed net coverage
#' @param coverage_opts options for the bed net coverage model
#' @param effect_sizes_name the name of a model for bed net effect sizes
#' @param effect_sizes_opts options for the bed net effect sizes model
#'
#' @return a **`ramp.xds`** model object
#' @export
setup_irs = function(xds_obj,
                         spray_houses_name = 'none', spray_houses_opts = list(),
                         effects_name = 'none', effects_opts = list(),
                         coverage_name = 'none', coverage_opts = list(),
                         effect_sizes_name = 'none', effect_sizes_opts = list()){

  xds_obj <- setup_vector_control(xds_obj)
  irs <- list()
  class(irs) <- "static"
  irs$coverage <- rep(0, xds_obj$nPatches)
  irs$eff_sz_obj <- list()
  xds_obj$irs_obj <- irs

  xds_obj <- setup_spray_houses(spray_houses_name, xds_obj, spray_houses_opts)
  xds_obj <- setup_irs_effects(effects_name, xds_obj, effects_opts)
  xds_obj <- setup_irs_coverage(coverage_name, xds_obj, coverage_opts)
  xds_obj <- setup_irs_effect_sizes(effect_sizes_name, xds_obj, 1, effect_sizes_opts)

  return(xds_obj)
}


#' @title Set the irs
#' @description Set the value of exogenous variables related to
#' irs
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`**  model object
#' @return a **`ramp.xds`** model object
#' @export
IRS <- function(t, xds_obj){
  UseMethod("IRS", xds_obj$irs_obj)
}

#' @title Set the irs
#' @description Set the value of exogenous variables related to
#' irs
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`**  model object
#' @return a **`ramp.xds`** model object
#' @export
IRS.none <- function(t, xds_obj){
  return(xds_obj)
}

#' @title Set the irs
#' @description Set the value of exogenous variables related to
#' irs
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`**  model object
#' @return a **`ramp.xds`** model object
#' @export
IRS.static <- function(t, xds_obj){
  return(xds_obj)
}

#' @title Set the irs
#' @description Set the value of exogenous variables related to
#' irs
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`**  model object
#' @return a **`ramp.xds`** model object
#' @export
IRS.dynamic <- function(t, xds_obj){
  xds_obj <- SprayHouses(t, xds_obj)
  xds_obj <- IRS_Effects(t, xds_obj)
}



