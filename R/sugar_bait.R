
#' @title Set no sugar baits
#' @description The null model for sugar baits
#' @inheritParams ramp.xds::SugarBaits
#' @return [list]
#' @export
SugarBaits.dynamic <- function(t, pars) {
  pars <- DistributeSugarBaits(t, pars)
  pars <- SugarBaitEffects(t, pars)
  pars <- SugarBaitCoverage(t, pars)
  return(pars)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param pars an **`xds`** object
#' @param distribute_sugar_baits_name the name of a model for mass bed net distribution
#' @param distribute_sugar_baits_opts options for the bed net distribution model
#' @param effects_name the name of a model for bed net effects
#' @param effects_opts options for the bed net effects model
#' @param coverage_name the name of a model for bed net coverage
#' @param coverage_opts options for the bed net coverage model
#' @param effectsizes_name the name of a model for bed net effect sizes
#' @param effectsizes_opts options for the bed net effect sizes model
#' @return an **`xds`** object
#' @export
xds_setup_sugar_baits = function(pars,
                        distribute_sugar_baits_name = 'none', distribute_sugar_baits_opts = list(),
                        effects_name = 'none', effects_opts = list(),
                        coverage_name = 'none', coverage_opts = list(),
                        effectsizes_name = 'none', effectsizes_opts = list()){
  pars = dynamic_vector_control(pars)
  SugarBaits <- list()
  class(SugarBaits) <- 'dynamic'
  pars$sugar_baits <- SugarBaits
  pars <- setup_distribute_sugar_baits(distribute_sugar_baits_name, pars, distribute_sugar_baits_opts)
  pars <- setup_sugar_bait_effects(effects_name, pars, effects_opts)
  pars <- setup_sugar_bait_coverage(coverage_name, pars, coverage_opts)
  pars <- setup_sugar_bait_effectsizes(effectsizes_name, pars, effectsizes_opts)
  return(pars)
}

