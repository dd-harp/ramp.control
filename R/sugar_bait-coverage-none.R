

#' @title No sugar baits
#' @description The null model for sugar baits
#' @inheritParams SugarBaitCoverage
#' @return [list]
#' @export
SugarBaitCoverage.none <- function(t, pars) {
  return(pars)
}

#' @title Set up no sugar bait coverage
#' @inheritParams setup_sugar_bait_coverage
#' @return an **`xds`** object
#' @export
setup_sugar_bait_coverage.none <- function(name='none', pars, opts=list()) {
  coverage <- 'none'
  class(coverage) <- 'none'
  pars$sugar_baits$coverage <- coverage
  return(pars)
}
