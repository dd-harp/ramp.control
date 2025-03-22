
#' @title No sugar baits
#' @description The null model for sugar baits
#' @inheritParams SugarBaitEffectSizes
#' @return [list]
#' @export
SugarBaitEffectSizes.none <- function(t, pars, s) {
  return(pars)
}
