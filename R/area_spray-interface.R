
#' @title Spray an Area
#' @description Set the value of exogenous variables related to
#' AreaSpray
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
AreaSpray <- function(t, pars) {
  UseMethod("AreaSpray", pars$area_spray)
}

#' @title Area Spraying
#' @description The null model for AreaSpray
#' @inheritParams AreaSpray
#' @return [list]
#' @export
AreaSpray.dynamic <- function(t, pars) {
  pars <- SprayArea(t, pars)
  pars <- AreaSprayEffects(t, pars)
  pars <- AreaSprayCoverage(t, pars)
  return(pars)
}

#' @title Set up area spraying
#' @description If the dynamic switch has not
#' already been set for Area Spraying, turn it on
#' and set up the modules.
#' @param pars an **`xds`** object
#' @param spray_area_name the name of a model for mass bed net distribution
#' @param spray_area_opts options for the bed net distribution model
#' @param effects_name the name of a model for bed net effects
#' @param effects_opts options for the bed net effects model
#' @param coverage_name the name of a model for bed net coverage
#' @param coverage_opts options for the bed net coverage model
#' @param effectsizes_name the name of a model for bed net effect sizes
#' @param effectsizes_opts options for the bed net effect sizes model
#' @return an **`xds`** object
#' @export
setup_area_spray = function(pars,
                            spray_area_name = 'none', spray_area_opts = list(),
                            effects_name = 'none', effects_opts = list(),
                            coverage_name = 'none', coverage_opts = list(),
                            effectsizes_name = 'none', effectsizes_opts = list()){
  pars = dynamic_vector_control(pars)
  AreaSprays <- list()
  class(AreaSprays) <- 'dynamic'
  pars <- setup_spray_area(spray_area_name, pars, spray_area_opts)
  pars <- setup_area_spray_effects(effects_name, pars, effects_opts)
  pars <- setup_area_spray_coverage(coverage_name, pars, coverage_opts)
  pars <- setup_area_spray_effectsizes(effectsizes_name, pars, effectsizes_opts)
  return(pars)
}

#' @title Set the spray_area_lsm
#' @description Set the value of exogenous variables related to
#' spray_area_lsm
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
SprayArea <- function(t, pars) {
  UseMethod("SprayArea", pars$area_spray$spray)
}

#' @title Set up dynamic lsm
#' @description If dynamic lsm has not
#' already been set up, then turn on dynamic
#' lsm and set all the
#' @param name the name of a model to set up
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @return an **`xds`** object
#' @export
setup_spray_area = function(name, pars, opts=list()){
  class(name) <- name
  UseMethod("setup_spray_area", name)
}

#' @title Set the area spray effects area spray
#' @description Set the value of exogenous variables related to
#' area spraying
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
AreaSprayEffects <- function(t, pars) {
  UseMethod("AreaSprayEffects", pars$area_spray$effects)
}

#' @title Set up dynamic area spray
#' @description If dynamic area spray has not
#' already been set up, then turn on dynamic
#' area spray and set all the
#' @param name the name of a model to set up
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @return an **`xds`** object
#' @export
setup_area_spray_effects = function(name, pars, opts=list()){
  class(name) <- name
  UseMethod("setup_area_spray_effects", name)
}

#' @title Set the area spray coverage
#' @description Set the value of exogenous variables related to
#' area spray coverage
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
AreaSprayCoverage <- function(t, pars) {
  UseMethod("AreaSprayCoverage", pars$area_spray$coverage)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param name the name of a model to set up
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @return an **`xds`** object
#' @export
setup_area_spray_coverage = function(name, pars, opts=list()){
  class(name) <- name
  UseMethod("setup_area_spray_coverage", name)
}

#' @title Set the AreaSprayEffectSizes
#' @description Set the value of exogenous variables related to
#' AreaSprayEffectSizes
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @param s vector species index
#' @return an **`xds`** object
#' @export
AreaSprayEffectSizes <- function(t, pars, s) {
  UseMethod("AreaSprayEffectSizes", pars$area_spray$effectsizes[[s]])
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param name the name of a model to set up
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @return an **`xds`** object
#' @export
setup_area_spray_effectsizes = function(name, pars, opts=list()){
  class(name) <- name
  UseMethod("setup_area_spray_effectsizes", name)
}




