
#' @title Set up "no irs_effectsizes"
#' @inheritParams setup_irs_effectsizes
#' @return an **`xds`** object
#' @export
setup_irs_effectsizes.none <- function(name, pars, opts) {
  effectsizes <- list()
  class(effectsizes) <- 'none'
  pars$irs$effectsizes <- effectsizes
  return(pars)
}
