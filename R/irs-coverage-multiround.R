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
  cover_obj <- list()
  cover_obj$class = "multiround"
  class(cover_obj) = "multiround"
  xds_obj$irs_obj$cover_obj <- cover_obj
  xds_obj <- setup_F_coverage_irs_multiround(xds_obj)
  return(xds_obj)
}

#' @title Make `F_coverage` for IRS
#'
#' @description Set up the IRS rounds
#'
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @return set up the rounds
#'
#' @export
setup_F_coverage_irs_multiround = function(xds_obj){
  xds_obj <- setup_irs_rounds(xds_obj, xds_obj$events_obj$irs$coverage)
  with(xds_obj$events_obj$irs,{
    rounds_par <- makepar_F_multiround(1, rounds)
    xds_obj$irs_obj$coverage_obj$F_cover = make_function(rounds_par)
    return(xds_obj)
  })}


#' @title Change IRS Multiround Coverage
#'
#' @description
#' With multi-round bed net coverage, there
#' is a different relationship between coverage
#' and coverage in each round
#'
#' @param xds_obj an **`xds`** model object
#' @param frac_sprayed the fraction of houses sprayed
#'
#' @export
change_irs_coverage_multiround = function(xds_obj, frac_sprayed){
  stopifnot(with(xds_obj, exists("events_obj")))
  stopifnot(with(xds_obj$events_obj, exists("irs")))
  stopifnot(length(frac_sprayed) == xds_obj$events_obj$irs$N)
  xds_obj$events_obj$irs$frac_sprayed <- frac_sprayed
  xds_obj <- setup_F_coverage_irs_multiround(xds_obj)
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

