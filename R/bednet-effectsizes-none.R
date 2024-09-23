

#' @title Set up "no bednet_effectsizes"
#' @inheritParams setup_bednet_effectsizes
#' @return an **`xds`** object
#' @export
setup_bednet_effectsizes.none <- function(name, pars, opts) {
  effectsizes <- 'none'
  class(effectsizes) <- 'none'
  pars$bednets$effectsizes <- effectsizes
  return(pars)
}
