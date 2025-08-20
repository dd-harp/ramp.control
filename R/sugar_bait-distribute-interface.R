
#' @title Set the distribute_sugar_baits_sugar_baits
#' @description Set the value of exogenous variables related to
#' distribute_sugar_baits_sugar_baits
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`** object
#' @return a **`ramp.xds`** object
#' @export
DistributeSugarBaits <- function(t, xds_obj) {
  UseMethod("DistributeSugarBaits", xds_obj$sugar_baits$distribute)
}


#' @title Set up dynamic sugar_baits
#' @description If dynamic sugar_baits has not
#' already been set up, then turn on dynamic
#' sugar_baits and set all the
#' @param name the name of a model to set up
#' @param xds_obj a **`ramp.xds`** object
#' @param opts a list of options to override defaults
#' @return a **`ramp.xds`** object
#' @export
setup_distribute_sugar_baits = function(name, xds_obj, opts=list()){
  class(name) <- name
  UseMethod("setup_distribute_sugar_baits", name)
}
