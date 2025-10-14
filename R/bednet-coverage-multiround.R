# Bed Net multiround

#' @title Set Up a Bed Net Coverage Function
#'
#' @description
#' This sets up a function to set the value of bed net
#' coverage
#'
#' @inheritParams setup_bednet_coverage
#' @export
setup_bednet_coverage.multiround = function(name="multiround", xds_obj, options=list()){
  class(xds_obj$vector_control_obj) = "dynamic"
  class(xds_obj$bednet_obj) = "dynamic"
  cover_obj <- list()
  cover_obj$class = "multiround"
  class(cover_obj) = "multiround"
  xds_obj$bednet_obj$cover_obj <- cover_obj
  cover = rep(1, xds_obj$events_obj$bednet$N)
  xds_obj$bednet_obj$cover_obj$F_cover <- make_bednet_multiround(cover, xds_obj)
  return(xds_obj)
}

#' @title Setup Muti-Round Bed Net coverage
#'
#' @description
#' With multi-round bed net coverage, there
#' is a different relationship between coverage
#' and coverage in each round
#'
#' @param coverage the coverage parametter
#' @param xds_obj an **`xds`** model object
#'
#' @export
change_bednet_coverage_multiround = function(coverage, xds_obj){
  stopifnot(with(xds_obj, exists("events_obj")))
  stopifnot(with(xds_obj$events_obj, exists("bednet")))
  stopifnot(length(coverage) == xds_obj$events_obj$bednet$N)
  xds_obj$events_obj$bednet$coverage = coverage
  contact = rep(1, xds_obj$events_obj$bednet$N)
  xds_obj$bednet_obj$coverage_obj <- make_bednet_multiround(contact, xds_obj)
  return(xds_obj)
}

#' @title Set no bednet_coverage
#' @description The null model for bednet_coverage
#' @inheritParams Bed_Net_Coverage
#' @return a **`xds`** object
#' @export
Bed_Net_Coverage.multiround <- function(t, y, xds_obj) {
  with(xds_obj$bednet_obj$cover_obj,{
    xds_obj$bednet_obj$coverage = F_cover(t)
    return(xds_obj)
})}


