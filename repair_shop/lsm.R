

#' @title Set the LSM
#' @description Set the value of exogenous variables related to
#' LSM
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
LSM <- function(t, xds_obj) {
  UseMethod("LSM", xds_obj$lsm)
}


#' @title Set no LSM
#' @description The null model for LSM
#' @inheritParams LSM
#' @return a **`ramp.xds`** model object
#' @export
LSM.none <- function(t, xds_obj) {
  return(xds_obj)
}


#' @title Set no LSM
#' @description The null model for LSM
#' @inheritParams LSM
#' @return a **`ramp.xds`** model object
#' @export
LSM.dynamic <- function(t, xds_obj) {
  xds_obj <- TreatHabitats(t, xds_obj)
  xds_obj <- LSMEffects(t, xds_obj)
  xds_obj <- LSMCoverage(t, xds_obj)
  return(xds_obj)
}



#' @title Set up "no LSM"
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
setup_no_lsm <- function(xds_obj) {
  NoLSM <- list()
  NoLSM$class <- 'none'
  class(NoLSM) <- 'none'
  xds_obj$lsm <- NoLSM
  xds_obj$lsm$coverage <- NoLSM
  xds_obj$lsm$effects <- NoLSM
  xds_obj$lsm$effectsizes <- list()
  xds_obj$lsm$effectsizes[[1]] <- NoLSM
  return(xds_obj)
}


#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param xds_obj a **`ramp.xds`** model object
#' @param treat_habitats_name the name of a model for mass LSM distribution
#' @param treat_habitats_opts options for the LSM distribution model
#' @param effects_name the name of a model for LSM effects
#' @param effects_opts options for the LSM effects model
#' @param coverage_name the name of a model for LSM coverage
#' @param coverage_opts options for the LSM coverage model
#' @param effectsizes_name the name of a model for LSM effect sizes
#' @param effectsizes_opts options for the LSM effect sizes model
#' @return a **`ramp.xds`** model object
#' @export
setup_lsm = function(xds_obj,
                        treat_habitats_name = 'none', treat_habitats_opts = list(),
                        effects_name = 'none', effects_opts = list(),
                        coverage_name = 'none', coverage_opts = list(),
                        effectsizes_name = 'none', effectsizes_opts = list()){
  xds_obj = dynamic_vector_control(xds_obj)
  LSM <- list()
  class(LSM) <- 'dynamic'
  xds_obj$lsm <- LSM
  xds_obj <- setup_treat_habitats(treat_habitats_name, xds_obj, treat_habitats_opts)
  xds_obj <- setup_lsm_effects(effects_name, xds_obj, effects_opts)
  xds_obj <- setup_lsm_coverage(coverage_name, xds_obj, coverage_opts)
  xds_obj <- setup_lsm_effectsizes(effectsizes_name, xds_obj, effectsizes_opts)
  return(xds_obj)
}

