# Irs Multi-Round configures F_cover

#' @title Set up dynamic forcing
#'
#' @description A set up utility function for
#' [IRS_Coverage].
#'
#' @inheritParams setup_irs_coverage
#'
#' @return a IRS coverage model object
#' @export
setup_irs_coverage.multiround = function(name, xds_obj, options=list()){
  class(xds_obj$vector_control_obj) = "dynamic"
  class(xds_obj$irs_obj) = "dynamic"
  xds_obj$irs_obj$cover_obj <- make_irs_multiround(xds_obj, use_contact=FALSE)
  return(xds_obj)
}

#' @title IRS Coverage with Multiple Rounds
#' @description A model for IRS coverage over time
#' when there have been several, different IRS models
#' @inheritParams IRS_Coverage
#' @return a **`ramp.xds`** model object
#' @export
IRS_Coverage.multiround <- function(t, y, xds_obj) {
  with(xds_obj$irs_obj$cover_obj,{
    xds_obj$irs_obj$coverage = F_cover(t)
    return(xds_obj)
})}

