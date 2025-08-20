
#' @title Set the SugarBaits
#' @description Set the value of exogenous variables related to
#' Sugar Baits
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
SugarBaits <- function(t, xds_obj) {
  UseMethod("SugarBaits", xds_obj$sugar_baits)
}

#' @title No sugar baits
#' @description The null model for sugar baits
#' @inheritParams SugarBaits
#' @return [list]
#' @export
SugarBaits.none <- function(t, xds_obj) {
  return(xds_obj)
}

#' @title Set no sugar baits
#' @description The null model for sugar baits
#' @inheritParams SugarBaits
#' @return [list]
#' @export
SugarBaits.dynamic <- function(t, xds_obj) {
  xds_obj <- DistributeSugarBaits(t, xds_obj)
  xds_obj <- SugarBaitEffects(t, xds_obj)
  xds_obj <- SugarBaitCoverage(t, xds_obj)
  return(xds_obj)
}


#' @title Set up no sugar baits
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
setup_no_sugar_baits <- function(xds_obj) {
  NoSugarBaits <- list()
  class(NoSugarBaits) <- 'none'
  xds_obj$sugar_baits <- NoSugarBaits
  xds_obj$sugar_baits$coverage <- NoSugarBaits
  xds_obj$sugar_baits$effectsizes <- list()
  xds_obj$sugar_baits$effectsizes[[1]] <- NoSugarBaits
  return(xds_obj)
}


#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param xds_obj a **`ramp.xds`** model object
#' @param distribute_sugar_baits_name the name of a model for mass bed net distribution
#' @param distribute_sugar_baits_opts options for the bed net distribution model
#' @param effects_name the name of a model for bed net effects
#' @param effects_opts options for the bed net effects model
#' @param coverage_name the name of a model for bed net coverage
#' @param coverage_opts options for the bed net coverage model
#' @param effectsizes_name the name of a model for bed net effect sizes
#' @param effectsizes_opts options for the bed net effect sizes model
#' @return a **`ramp.xds`** model object
#' @export
setup_sugar_baits = function(xds_obj,
                        distribute_sugar_baits_name = 'none', distribute_sugar_baits_opts = list(),
                        effects_name = 'none', effects_opts = list(),
                        coverage_name = 'none', coverage_opts = list(),
                        effectsizes_name = 'none', effectsizes_opts = list()){
  xds_obj = dynamic_vector_control(xds_obj)
  SugarBaits <- list()
  class(SugarBaits) <- 'dynamic'
  xds_obj$sugar_baits <- SugarBaits
  xds_obj <- setup_distribute_sugar_baits(distribute_sugar_baits_name, xds_obj, distribute_sugar_baits_opts)
  xds_obj <- setup_sugar_bait_effects(effects_name, xds_obj, effects_opts)
  xds_obj <- setup_sugar_bait_coverage(coverage_name, xds_obj, coverage_opts)
  xds_obj <- setup_sugar_bait_effectsizes(effectsizes_name, xds_obj, effectsizes_opts)
  return(xds_obj)
}

