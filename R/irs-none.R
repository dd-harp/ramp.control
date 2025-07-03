
#' @title Set no irs
#' @description The null model for irs
#' @inheritParams IRS
#' @return an **`xds`** object
#' @export
IRS.none <- function(t, pars) {
  return(pars)
}

#' @title Set up "no irs"
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
setup_no_irs <- function(pars) {
  no_irs <- list()
  class(no_irs) = 'none'
  no_irs$name = 'none'
  pars$irs <- no_irs
  pars$irs$spray_mod <- no_irs
  pars$irs$effects_mod <- no_irs
  pars$irs$coverage_mod <- no_irs

  pars$irs$ef_sz_mod <- list()
  pars$irs$ef_sz_mod[[1]] <- no_irs
  return(pars)
}

#' @title Set no spray_houses
#' @description The null model for spray_houses
#' @inheritParams SprayHouses
#' @return [list]
#' @export
SprayHouses.none <- function(t, pars) {
  return(pars)
}

#' @title Set up "no spray_houses"
#' @inheritParams setup_spray_houses
#' @return an irs spray model object
#' @export
setup_spray_houses.none <- function(name, pars, opts=list()) {
  spray_houses <- list()
  spray_houses$name <- "none"
  class(spray_houses) <- "none"
  return(spray_houses)
}

#' @title Set no irs_effects
#' @description The null model for irs_effects
#' @inheritParams IRSEffects
#' @return an **`xds`** object
#' @export
IRSEffects.none <- function(t, pars) {
  return(pars)
}

#' @title Set up "no irs_effects"
#' @inheritParams setup_irs_effects
#' @return an irs effects model object
#' @export
setup_irs_effects.none <- function(name, pars, opts=list()) {
  effects <- 'none'
  class(effects) <- 'none'
  return(effects)
}


#' @title Set no irs
#' @description The null model for irs
#' @inheritParams IRSCoverage
#' @return an **`xds`** object
#' @export
IRSCoverage.none <- function(t, pars) {
  return(pars)
}

#' @title Set up "no irs_coverage"
#' @inheritParams setup_irs_coverage
#' @return an irs coverage model object
#' @export
setup_irs_coverage.none <- function(name='none', pars, opts=list()) {
  coverage <- 'none'
  class(coverage) <- 'none'
  return(coverage)
}


#' @title Set no irs_effectsizes
#' @description The null model for irs_effectsizes
#' @inheritParams IRSEffectSizes
#' @return an **`xds`** object
#' @export
IRSEffectSizes.none <- function(t, pars, s) {
  return(pars)
}

#' @title Set up "no irs_effectsizes"
#' @inheritParams setup_irs_effectsizes
#' @return an irs effect size model object
#' @export
setup_irs_effectsizes.none <- function(name, pars, s, opts) {
  effectsizes <- list()
  effectsizes$class <- 'none'
  class(effectsizes) <- 'none'
  ef_sz_mod <- list()
  ef_sz_mod[[1]] <- effectsizes
  return(ef_sz_mod)
}
