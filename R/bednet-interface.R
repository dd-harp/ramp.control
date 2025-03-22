
#' @title Set the bednet
#' @description Set the value of exogenous variables related to
#' bednet
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
BedNet <- function(t, pars) {
  UseMethod("BedNet", pars$bednets)
}


#' @title Set no bednet
#' @description The null model for bednet
#' @inheritParams BedNet
#' @return [list]
#' @export
BedNet.dynamic <- function(t, pars) {
   pars <- DistributeBedNets(t, pars)
   pars <- OwnBedNets(t, pars)
   pars <- UseBedNets(t, pars)
   pars <- BedNetEffects(t, pars)
   pars <- BedNetCoverage(t, pars)
   return(pars)
}



#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param pars an **`xds`** object
#' @param distribute_name the name of a model for mass bed net distribution
#' @param distribute_opts options for the bed net distribution model
#' @param own_name the name of a model for bed net ownership
#' @param own_opts options for the bed net ownership model
#' @param use_name the name of a model for bed net usage
#' @param use_opts options for the bed net usage model
#' @param effects_name the name of a model for bed net effects
#' @param effects_opts options for the bed net effects model
#' @param coverage_name the name of a model for bed net coverage
#' @param coverage_opts options for the bed net coverage model
#' @param effectsizes_name the name of a model for bed net effect sizes
#' @param effectsizes_opts options for the bed net effect sizes model
#' @return an **`xds`** object
#' @export
setup_bednets = function(pars,
                        distribute_name = 'none', distribute_opts = list(),
                        own_name = 'none', own_opts = list(),
                        use_name = 'none', use_opts = list(),
                        effects_name = 'none', effects_opts = list(),
                        coverage_name = 'none', coverage_opts = list(),
                        effectsizes_name = 'none', effectsizes_opts = list()){

  pars <- dynamic_vector_control(pars)
  bednets <- list()
  bednets$name <- 'dynamic'
  class(bednets) <- 'dynamic'
  pars$bednets <- bednets


  pars <- setup_distribute_bednets(distribute_name, pars, distribute_opts)
  pars <- setup_own_bednets(own_name, pars, own_opts)
  pars <- setup_use_bednets(use_name, pars, use_opts)
  pars <- setup_bednet_effects(effects_name, pars, effects_opts)
  pars <- setup_bednet_coverage(coverage_name, pars, coverage_opts)
  pars <- setup_bednet_effectsizes(effectsizes_name, pars, effectsizes_opts)
  return(pars)
}


#' @title Set the distribute_bednets
#' @description Set the value of exogenous variables related to
#' distribute_bednets
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
DistributeBedNets <- function(t, pars) {
  UseMethod("DistributeBedNets", pars$bednets$distribute)
}

#' @title Set up dynamic bednets
#' @description If dynamic bednets has not
#' already been set up, then turn on dynamic
#' bednets and set all the
#' @param name the name of a model to set up
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @return an **`xds`** object
#' @export
setup_distribute_bednets = function(name, pars, opts=list()){
  class(name) <- name
  UseMethod("setup_distribute_bednets", name)
}


#' @title Set the own_bednets
#' @description Set the value of exogenous variables related to
#' own_bednets
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
OwnBedNets <- function(t, pars) {
  UseMethod("OwnBedNets", pars$bednets$own)
}

#' @title Set up dynamic bednets
#' @description If dynamic bednets has not
#' already been set up, then turn on dynamic
#' bednets and set all the
#' @param name the name of a model to set up
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @return an **`xds`** object
#' @export
setup_own_bednets = function(name, pars, opts=list()){
  class(name) <- name
  UseMethod("setup_own_bednets", name)
}



#' @title Set the use_bednets
#' @description Set the value of exogenous variables related to
#' use_bednets
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
UseBedNets <- function(t, pars) {
  UseMethod("UseBedNets", pars$bednets$use)
}

#' @title Set up dynamic bednets
#' @description If dynamic bednets has not
#' already been set up, then turn on dynamic
#' bednets and set all the
#' @param name the name of a model to set up
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @return an **`xds`** object
#' @export
setup_use_bednets = function(name, pars, opts=list()){
  class(name) <- name
  UseMethod("setup_use_bednets", name)
}


#' @title Set the bednet
#' @description Set the value of exogenous variables related to
#' bednet
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
BedNetCoverage <- function(t, pars) {
  UseMethod("BedNetCoverage", pars$bednets$coverage)
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
setup_bednet_coverage = function(name, pars, opts=list()){
  class(name) <- name
  UseMethod("setup_bednet_coverage", name)
}


#' @title Set the bednet_effects
#' @description Set the value of exogenous variables related to
#' bednet_effects
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
BedNetEffects <- function(t, pars) {
  UseMethod("BedNetEffects", pars$bednets$effects)
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
setup_bednet_effects = function(name, pars, opts=list()){
  class(name) <- name
  UseMethod("setup_bednet_effects", name)
}


#' @title Set the bednet_effectsizes
#' @description Set the value of exogenous variables related to
#' bednet_effectsizes
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return an **`xds`** object
#' @export
BedNetEffectSizes <- function(t, pars, s) {
  UseMethod("BedNetEffectSizes", pars$bednets$effectsizes)
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
setup_bednet_effectsizes = function(name, pars, opts=list()){
  class(name) <- name
  UseMethod("setup_bednet_effectsizes", name)
}

