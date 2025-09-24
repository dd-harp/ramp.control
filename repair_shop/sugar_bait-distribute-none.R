
#' @title Distribute no sugar baits
#' @description The model for no sugar bait distribution
#' @inheritParams DistributeSugarBaits
#' @return a **`ramp.xds`** model object
#' @export
DistributeSugarBaits.none <- function(t, xds_obj) {
  return(xds_obj)
}

#' @title Set up distribution for the no sugar baits model
#' @inheritParams setup_distribute_sugar_baits
#' @return a **`xds`** object
#' @export
setup_distribute_sugar_baits.none <- function(name, xds_obj, opts) {
  distribute_sugar_baits <- 'none'
  class(distribute_sugar_baits) <- 'none'
  xds_obj$sugar_baits$distribute <- distribute_sugar_baits
  return(xds_obj)
}
