
#' @title Implement IRS
#' @description Set the value of exogenous variables related to
#' IRS
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
IRS <- function(t, xds_obj) {
  UseMethod("IRS", xds_obj$irs)
}

#' @title Set no irs
#' @description The null model for irs
#' @inheritParams IRS
#' @return [list]
#' @export
IRS.dynamic <- function(t, xds_obj) {
  xds_obj <- SprayHouses(t, xds_obj)
  xds_obj <- IRSEffects(t, xds_obj)
  return(xds_obj)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param xds_obj a **`ramp.xds`** model object
#' @param spray_houses_name the name of a model for mass bed net distribution
#' @param spray_houses_opts options for the bed net distribution model
#' @param effects_name the name of a model for bed net effects
#' @param effects_opts options for the bed net effects model
#' @param coverage_name the name of a model for bed net coverage
#' @param coverage_opts options for the bed net coverage model
#' @param effect_sizes_name the name of a model for bed net effect sizes
#' @param effect_sizes_opts options for the bed net effect sizes model
#' @return a **`ramp.xds`** model object
#' @export
setup_irs = function(xds_obj,
                        spray_houses_name = 'none', spray_houses_opts = list(),
                        effects_name = 'none', effects_opts = list(),
                        coverage_name = 'none', coverage_opts = list(),
                        effect_sizes_name = 'none', effect_sizes_opts = list()){
  xds_obj = dynamic_vector_control(xds_obj)
  irs = list()
  irs$name = "dynamic"
  class(irs) = "dynamic"
  xds_obj$irs = irs

  # Spraying, coverage, and generic effects
  xds_obj$irs$spray_mod <- setup_spray_houses(spray_houses_name, xds_obj, spray_houses_opts)
  xds_obj$irs$effects_mod <- setup_irs_effects(effects_name, xds_obj, effects_opts)
  xds_obj$irs$coverage_mod <-  setup_irs_coverage(coverage_name, xds_obj, coverage_opts)
  xds_obj$irs$coverage <- list()

  # The "effect sizes" model for the first vector species
  xds_obj$irs$ef_sz_mod <- list()
  xds_obj$irs$ef_sz_mod[[1]] <- setup_irs_effect_sizes(effect_sizes_name, xds_obj, effect_sizes_opts)

  return(xds_obj)
}

#' @title Set the spray_houses_irs
#' @description Set the value of exogenous variables related to
#' spray_houses_irs
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
SprayHouses <- function(t, xds_obj) {
  UseMethod("SprayHouses", xds_obj$irs$spray_mod)
}

#' @title Set up dynamic irs
#' @description If dynamic irs has not
#' already been set up, then turn on dynamic
#' irs and set all the
#' @param name the name of a model to set up
#' @param xds_obj a **`ramp.xds`** model object
#' @param opts a list of options to override defaults
#' @return a **`ramp.xds`** model object
#' @export
setup_spray_houses = function(name, xds_obj, opts=list()){
  class(name) <- name
  UseMethod("setup_spray_houses", name)
}


#' @title Set the irs_effects_irs
#' @description Set the value of exogenous variables related to
#' irs_effects_irs
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
IRSEffects <- function(t, xds_obj) {
  UseMethod("IRSEffects", xds_obj$irs$effects_mod)
}


#' @title Set up dynamic irs
#' @description If dynamic irs has not
#' already been set up, then turn on dynamic
#' irs and set all the
#' @param name the name of a model to set up
#' @param xds_obj a **`ramp.xds`** model object
#' @param opts a list of options to override defaults
#' @return a **`ramp.xds`** model object
#' @export
setup_irs_effects = function(name, xds_obj, opts=list()){
  class(name) <- name
  UseMethod("setup_irs_effects", name)
}


#' @title Implement IRSCoverage
#' @description Set the value of exogenous variables related to
#' IRSCoverage
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
IRSCoverage <- function(t, xds_obj) {
  UseMethod("IRSCoverage", xds_obj$irs$coverage_mod)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#'
#' @param name the name of a model to set up
#' @param xds_obj a **`ramp.xds`** model object
#' @param opts a list of options to override defaults
#'
#' @return a IRS coverage model object
#'
#' @export
setup_irs_coverage = function(name, xds_obj, opts=list()){
  class(name) <- name
  UseMethod("setup_irs_coverage", name)
}


#' @title Set the irs_effectsizes
#' @description Set the value of exogenous variables related to
#' irs_effectsizes
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`** model object
#' @param s vector species index
#' @return a **`ramp.xds`** model object
#' @export
IRSEffectSizes <- function(t, xds_obj, s=1) {
  UseMethod("IRSEffectSizes", xds_obj$irs$ef_sz_mod[[s]])
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param name the name of a model to set up
#' @param xds_obj a **`ramp.xds`** model object
#' @param opts a list of options to override defaults
#' @return a irs effect size model object
#' @export
setup_irs_effect_sizes = function(name, xds_obj, opts=list()){
  class(name) <- name
  UseMethod("setup_irs_effect_sizes", name)
}

