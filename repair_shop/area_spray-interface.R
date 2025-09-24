
#' @title Spray an Area
#' @description Set the value of exogenous variables related to
#' AreaSpray
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`** object
#' @return a **`ramp.xds`** object
#' @export
AreaSpray <- function(t, xds_obj) {
  UseMethod("AreaSpray", xds_obj$area_spray)
}

#' @title Area Spraying
#' @description The null model for AreaSpray
#' @inheritParams AreaSpray
#' @return [list]
#' @export
AreaSpray.dynamic <- function(t, xds_obj) {
  xds_obj <- SprayArea(t, xds_obj)
  xds_obj <- AreaSprayEffects(t, xds_obj)
  xds_obj <- AreaSprayCoverage(t, xds_obj)
  return(xds_obj)
}

#' @title Set up area spraying
#' @description If the dynamic switch has not
#' already been set for Area Spraying, turn it on
#' and set up the modules.
#' @param xds_obj a **`ramp.xds`** object
#' @param spray_area_name the name of a model for mass bed net distribution
#' @param spray_area_opts options for the bed net distribution model
#' @param effects_name the name of a model for bed net effects
#' @param effects_opts options for the bed net effects model
#' @param coverage_name the name of a model for bed net coverage
#' @param coverage_opts options for the bed net coverage model
#' @param effectsizes_name the name of a model for bed net effect sizes
#' @param effectsizes_opts options for the bed net effect sizes model
#' @return a **`ramp.xds`** object
#' @export
setup_area_spray = function(xds_obj,
                            spray_area_name = 'none', spray_area_opts = list(),
                            effects_name = 'none', effects_opts = list(),
                            coverage_name = 'none', coverage_opts = list(),
                            effectsizes_name = 'none', effectsizes_opts = list()){
  xds_obj = dynamic_vector_control(xds_obj)
  AreaSprays <- list()
  class(AreaSprays) <- 'dynamic'
  xds_obj <- setup_spray_area(spray_area_name, xds_obj, spray_area_opts)
  xds_obj <- setup_area_spray_effects(effects_name, xds_obj, effects_opts)
  xds_obj <- setup_area_spray_coverage(coverage_name, xds_obj, coverage_opts)
  xds_obj <- setup_area_spray_effectsizes(effectsizes_name, xds_obj, effectsizes_opts)
  return(xds_obj)
}

#' @title Set the spray_area_lsm
#' @description Set the value of exogenous variables related to
#' spray_area_lsm
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`** object
#' @return a **`ramp.xds`** object
#' @export
SprayArea <- function(t, xds_obj) {
  UseMethod("SprayArea", xds_obj$area_spray$spray)
}

#' @title Set up dynamic lsm
#' @description If dynamic lsm has not
#' already been set up, then turn on dynamic
#' lsm and set all the
#' @param name the name of a model to set up
#' @param xds_obj a **`ramp.xds`** object
#' @param opts a list of options to override defaults
#' @return a **`ramp.xds`** object
#' @export
setup_spray_area = function(name, xds_obj, opts=list()){
  class(name) <- name
  UseMethod("setup_spray_area", name)
}

#' @title Set the area spray effects area spray
#' @description Set the value of exogenous variables related to
#' area spraying
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`** object
#' @return a **`ramp.xds`** object
#' @export
AreaSprayEffects <- function(t, xds_obj) {
  UseMethod("AreaSprayEffects", xds_obj$area_spray$effects)
}

#' @title Set up dynamic area spray
#' @description If dynamic area spray has not
#' already been set up, then turn on dynamic
#' area spray and set all the
#' @param name the name of a model to set up
#' @param xds_obj a **`ramp.xds`** object
#' @param opts a list of options to override defaults
#' @return a **`ramp.xds`** object
#' @export
setup_area_spray_effects = function(name, xds_obj, opts=list()){
  class(name) <- name
  UseMethod("setup_area_spray_effects", name)
}

#' @title Set the area spray coverage
#' @description Set the value of exogenous variables related to
#' area spray coverage
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`** object
#' @return a **`ramp.xds`** object
#' @export
AreaSprayCoverage <- function(t, xds_obj) {
  UseMethod("AreaSprayCoverage", xds_obj$area_spray$coverage)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param name the name of a model to set up
#' @param xds_obj a **`ramp.xds`** object
#' @param opts a list of options to override defaults
#' @return a **`ramp.xds`** object
#' @export
setup_area_spray_coverage = function(name, xds_obj, opts=list()){
  class(name) <- name
  UseMethod("setup_area_spray_coverage", name)
}

#' @title Set the AreaSprayEffectSizes
#' @description Set the value of exogenous variables related to
#' AreaSprayEffectSizes
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`** object
#' @param s vector species index
#' @return a **`ramp.xds`** object
#' @export
AreaSprayEffectSizes <- function(t, xds_obj, s) {
  UseMethod("AreaSprayEffectSizes", xds_obj$area_spray$effectsizes[[s]])
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param name the name of a model to set up
#' @param xds_obj a **`ramp.xds`** object
#' @param opts a list of options to override defaults
#' @return a **`ramp.xds`** object
#' @export
setup_area_spray_effectsizes = function(name, xds_obj, opts=list()){
  class(name) <- name
  UseMethod("setup_area_spray_effectsizes", name)
}




