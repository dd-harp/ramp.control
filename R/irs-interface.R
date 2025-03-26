
#' @title Implement IRS
#' @description Set the value of exogenous variables related to
#' IRS
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
IRS <- function(t, pars) {
  UseMethod("IRS", pars$irs)
}


#' @title Set no irs
#' @description The null model for irs
#' @inheritParams IRS
#' @return [list]
#' @export
IRS.dynamic <- function(t, pars) {
  pars <- SprayHouses(t, pars)
  pars <- IRSEffects(t, pars)
  return(pars)
}



#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param pars an **`xds`** object
#' @param spray_houses_name the name of a model for mass bed net distribution
#' @param spray_houses_opts options for the bed net distribution model
#' @param effects_name the name of a model for bed net effects
#' @param effects_opts options for the bed net effects model
#' @param coverage_name the name of a model for bed net coverage
#' @param coverage_opts options for the bed net coverage model
#' @param effectsizes_name the name of a model for bed net effect sizes
#' @param effectsizes_opts options for the bed net effect sizes model
#' @return an **`xds`** object
#' @export
setup_irs = function(pars,
                        spray_houses_name = 'none', spray_houses_opts = list(),
                        effects_name = 'none', effects_opts = list(),
                        coverage_name = 'none', coverage_opts = list(),
                        effectsizes_name = 'none', effectsizes_opts = list()){
  pars = dynamic_vector_control(pars)
  irss <- list()
  class(irss) <- 'dynamic'
  pars <- setup_spray_houses(spray_houses_name, pars, spray_houses_opts)
  pars <- setup_irs_effects(effects_name, pars, effects_opts)
  pars <- setup_irs_coverage(coverage_name, pars, coverage_opts)
  pars <- setup_irs_effectsizes(effectsizes_name, pars, effectsizes_opts)
  return(pars)
}

#' @title Set the spray_houses_irs
#' @description Set the value of exogenous variables related to
#' spray_houses_irs
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
SprayHouses <- function(t, pars) {
  UseMethod("SprayHouses", pars$irs$spray_houses)
}

#' @title Set up dynamic irs
#' @description If dynamic irs has not
#' already been set up, then turn on dynamic
#' irs and set all the
#' @param name the name of a model to set up
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @return an **`xds`** object
#' @export
setup_spray_houses = function(name, pars, opts=list()){
  class(name) <- name
  UseMethod("setup_spray_houses", name)
}


#' @title Set the irs_effects_irs
#' @description Set the value of exogenous variables related to
#' irs_effects_irs
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
IRSEffects <- function(t, pars) {
  UseMethod("IRSEffects", pars$irs$effects)
}


#' @title Set up dynamic irs
#' @description If dynamic irs has not
#' already been set up, then turn on dynamic
#' irs and set all the
#' @param name the name of a model to set up
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @return an **`xds`** object
#' @export
setup_irs_effects = function(name, pars, opts=list()){
  class(name) <- name
  UseMethod("setup_irs_effects", name)
}


#' @title Implement IRSCoverage
#' @description Set the value of exogenous variables related to
#' IRSCoverage
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
IRSCoverage <- function(t, pars) {
  UseMethod("IRSCoverage", pars$irs$coverage)
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
setup_irs_coverage = function(name, pars, opts=list()){
  class(name) <- name
  UseMethod("setup_irs_coverage", name)
}


#' @title Set the irs_effectsizes
#' @description Set the value of exogenous variables related to
#' irs_effectsizes
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @param s vector species index
#' @return an **`xds`** object
#' @export
IRSEffectSizes <- function(t, pars, s) {
  UseMethod("IRSEffectSizes", pars$irs$effectsizes[[s]])
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
setup_irs_effectsizes = function(name, pars, opts=list()){
  class(name) <- name
  UseMethod("setup_irs_effectsizes", name)
}

