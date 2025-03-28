
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
  irs <- list()
  class(irs) = 'none'
  irs$name = 'none'
  pars$irs <- irs
  pars$irs$coverage <- irs
  pars$irs$effectsizes <- list()
  pars$irs$effectsizes[[1]] <- irs
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
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
setup_spray_houses.none <- function(name, pars, opts=list()) {
  spray_houses <- 'none'
  class(spray_houses) <- 'none'
  pars$irs$spray_houses <- spray_houses
  return(pars)
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
#' @return an **`xds`** object
#' @export
setup_irs_effects.none <- function(name, pars, opts=list()) {
  effects <- 'none'
  class(effects) <- 'none'
  pars$irs$effects <- effects
  return(pars)
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
#' @return an **`xds`** object
#' @export
setup_irs_coverage.none <- function(name='none', pars, opts=list()) {
  coverage <- 'none'
  class(coverage) <- 'none'
  pars$irs$coverage <- coverage
  return(pars)
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
#' @return an **`xds`** object
#' @export
setup_irs_effectsizes.none <- function(name, pars, opts) {
  effectsizes <- list()
  effectsizes$class <- 'none'
  class(effectsizes) <- 'none'
  pars$irs$effectsizes <- list()
  pars$irs$effectsizes[[1]] <- effectsizes
  return(pars)
}
