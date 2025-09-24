
#' @title Set no treat_habitats
#' @description The null model for treat_habitats
#' @inheritParams TreatHabitats
#' @return a **`ramp.xds`** model object
#' @export
TreatHabitats.none <- function(t, xds_obj) {
  return(xds_obj)
}

#' @title Set up "no treat_habitats"
#' @inheritParams setup_treat_habitats
#' @return a **`ramp.xds`** model object
#' @export
setup_treat_habitats.none <- function(name, xds_obj, opts=list()) {
  treat_habitats <- 'none'
  class(treat_habitats) <- 'none'
  xds_obj$lsm$treat_habitats <- treat_habitats
  return(xds_obj)
}
