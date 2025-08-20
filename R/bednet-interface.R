
#' @title Set the bednet
#' @description Set the value of exogenous variables related to
#' bednet
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`**  model object
#' @return a **`ramp.xds`** model object
#' @export
BedNet <- function(t, xds_obj) {
  UseMethod("BedNet", xds_obj$bednets)
}


#' @title Set no bednet
#' @description The null model for bednet
#' @inheritParams BedNet
#' @return [list]
#' @export
BedNet.dynamic <- function(t, xds_obj) {
  xds_obj <- DistributeBedNets(t, xds_obj)
  xds_obj <- OwnBedNets(t, xds_obj)
  xds_obj <- UseBedNets(t, xds_obj)
  xds_obj <- BedNetEffects(t, xds_obj)
#   xds_obj <- BedNetCoverage(t, xds_obj)
   return(xds_obj)
}



#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param xds_obj a **`ramp.xds`**  model object
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
#' @param effect_sizes_name the name of a model for bed net effect sizes
#' @param effect_sizes_opts options for the bed net effect sizes model
#' @return a **`ramp.xds`** model object
#' @export
setup_bednets = function(xds_obj,
                        distribute_name = 'none', distribute_opts = list(),
                        own_name = 'none', own_opts = list(),
                        use_name = 'none', use_opts = list(),
                        effects_name = 'none', effects_opts = list(),
                        coverage_name = 'none', coverage_opts = list(),
                        effect_sizes_name = 'none', effect_sizes_opts = list()){

  xds_obj <- dynamic_vector_control(xds_obj)
  bednets <- list()
  bednets$name <- 'dynamic'
  class(bednets) <- 'dynamic'
  bednets$coverage <- rep(0, xds_obj$nPatches)
  xds_obj$bednets <- bednets

  xds_obj$bednets$distribute_mod <- setup_distribute_bednets(distribute_name, xds_obj, distribute_opts)
  xds_obj$bednets$owner_mod <- setup_own_bednets(own_name, xds_obj, own_opts)
  xds_obj$bednets$user_mod  <- setup_use_bednets(use_name, xds_obj, use_opts)
  xds_obj$bednets$effects_mod <- setup_bednet_effects(effects_name, xds_obj, effects_opts)
  xds_obj$bednets$coverage_mod <- setup_bednet_coverage(coverage_name, xds_obj, coverage_opts)
  xds_obj$bednets$ef_sz_mod <- list()
  xds_obj$bednets$ef_sz_mod[[1]] <- setup_bednet_effect_sizes(effect_sizes_name, xds_obj, effect_sizes_opts)
  return(xds_obj)
}


#' @title Set the distribute_bednets
#' @description Set the value of exogenous variables related to
#' distribute_bednets
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
DistributeBedNets <- function(t, xds_obj) {
  UseMethod("DistributeBedNets", xds_obj$bednets$distribute_mod)
}

#' @title Set up dynamic bednets
#' @description If dynamic bednets has not
#' already been set up, then turn on dynamic
#' bednets and set all the
#' @param name the name of a model to set up
#' @param xds_obj a **`ramp.xds`**  model object
#' @param opts a list of options to override defaults
#' @return a **`ramp.xds`** model object
#' @export
setup_distribute_bednets = function(name, xds_obj, opts=list()){
  class(name) <- name
  UseMethod("setup_distribute_bednets", name)
}


#' @title Set the own_bednets
#' @description Set the value of exogenous variables related to
#' own_bednets
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`**  model object
#' @return a **`ramp.xds`** model object
#' @export
OwnBedNets <- function(t, xds_obj) {
  UseMethod("OwnBedNets", xds_obj$bednets$owner_mod)
}

#' @title Set up dynamic bednets
#' @description If dynamic bednets has not
#' already been set up, then turn on dynamic
#' bednets and set all the
#' @param name the name of a model to set up
#' @param xds_obj a **`ramp.xds`**  model object
#' @param opts a list of options to override defaults
#' @return a **`ramp.xds`** model object
#' @export
setup_own_bednets = function(name, xds_obj, opts=list()){
  class(name) <- name
  UseMethod("setup_own_bednets", name)
}



#' @title Set the use_bednets
#' @description Set the value of exogenous variables related to
#' use_bednets
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`**  model object
#' @return a **`ramp.xds`** model object
#' @export
UseBedNets <- function(t, xds_obj) {
  UseMethod("UseBedNets", xds_obj$bednets$user_mod)
}

#' @title Set up dynamic bednets
#' @description If dynamic bednets has not
#' already been set up, then turn on dynamic
#' bednets and set all the
#' @param name the name of a model to set up
#' @param xds_obj a **`ramp.xds`**  model object
#' @param opts a list of options to override defaults
#' @return a **`ramp.xds`** model object
#' @export
setup_use_bednets = function(name, xds_obj, opts=list()){
  class(name) <- name
  UseMethod("setup_use_bednets", name)
}


#' @title Set the bednet
#' @description Set the value of exogenous variables related to
#' bednet
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`**  model object
#' @return a **`ramp.xds`** model object
#' @export
BedNetCoverage <- function(t, xds_obj) {
  UseMethod("BedNetCoverage", xds_obj$bednets$coverage_mod)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param name the name of a model to set up
#' @param xds_obj a **`ramp.xds`**  model object
#' @param opts a list of options to override defaults
#' @return a **`ramp.xds`** model object
#' @export
setup_bednet_coverage = function(name, xds_obj, opts=list()){
  class(name) <- name
  UseMethod("setup_bednet_coverage", name)
}


#' @title Set the bednet_effects
#' @description Set the value of exogenous variables related to
#' bednet_effects
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`**  model object
#' @return a **`ramp.xds`** model object
#' @export
BedNetEffects <- function(t, xds_obj) {
  UseMethod("BedNetEffects", xds_obj$bednets$effects_mod)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param name the name of a model to set up
#' @param xds_obj a **`ramp.xds`**  model object
#' @param opts a list of options to override defaults
#' @return a **`ramp.xds`** model object
#' @export
setup_bednet_effects = function(name, xds_obj, opts=list()){
  class(name) <- name
  UseMethod("setup_bednet_effects", name)
}


#' @title Copmute bednet effect sizes
#' @description Compute the effect sizes
#' associated with bed nets
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`**  model object
#' @param s the vector species index
#' @return a **`ramp.xds`** model object
#' @export
BedNetEffectSizes <- function(t, xds_obj, s) {
  UseMethod("BedNetEffectSizes", xds_obj$bednets$ef_sz_mod[[s]])
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param name the name of a model to set up
#' @param xds_obj a **`ramp.xds`**  model object
#' @param opts a list of options to override defaults
#' @return a **`ramp.xds`** model object
#' @export
setup_bednet_effect_sizes = function(name, xds_obj, opts=list()){
  class(name) <- name
  UseMethod("setup_bednet_effect_sizes", name)
}

