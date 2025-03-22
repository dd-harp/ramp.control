

#' @title Set no LSMCoverage
#' @description The null model for LSMCoverage
#' @inheritParams LSMCoverage
#' @return an **`xds`** object
#' @export
LSMCoverage.none <- function(t, pars) {
  return(pars)
}


#' @title Set up "no lsm_coverage"
#' @inheritParams setup_lsm_coverage
#' @return an **`xds`** object
#' @export
setup_lsm_coverage.none <- function(name='none', pars, opts=list()) {
  coverage <- 'none'
  class(coverage) <- 'none'
  pars$lsm$coverage <- coverage
  return(pars)
}
