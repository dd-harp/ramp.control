# Irs Multi-Round configures F_cover

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
  xds_obj$irs_obj
  xds_obj$irs_obj$cover_obj <- make_irs_multiround(options)
  return(xds_obj)
}

#' @title Set up dynamic forcing
#'
#' @description Set up a function that computes
#' irs coverage over time.
#'
#' @param options a list of options to override defaults
#' @param t_init the time when IRS started
#' @param coverage the coverage achieved
#' @param type the IRS type
#' @param zap the contact parameter
#'
#' @return a IRS coverage model object
#' @export
make_irs_multiround = function(options=list(),
                                t_init = 0,
                                coverage = 0,
                                type = "none",
                                zap = 1){
  with(options,{
    nRounds <- length(t_init)
    stopifnot(length(coverage) == nRounds)
    stopifnot(length(type) == nRounds)

    cover <- list()
    cover$nRounds = nRounds
    class(cover) <- "multiround"
    cover$t_init = t_init
    cover$coverage = coverage
    cover$type = type
    cover$zap = checkIt(zap, nRounds)

    cover = setup_F_cover_irs(cover)

    return(cover)
})}


#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param cover a coverage model object
#' @return a **`ramp.xds`** model object
#' @export
setup_F_cover_irs = function(cover){

  rounds <- list()
  if(cover$nRounds==0) rounds[[1]] = makepar_F_zero()
  if(cover$nRounds>0)
    for(i in 1:cover$nRounds)
      rounds[[i]] = with(cover, make_irs_round(type[i], t_init[i], coverage[i], zap[i]))

  rounds_par <- makepar_F_multiround(cover$nRounds, rounds)
  cover$rounds <- rounds
  cover$rounds_par <- rounds_par
  cover$F_cover <- make_function(rounds_par)

  return(cover)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param xds_obj the `ramp.xds` model object
#' @param type the name of the IRS type
#' @param t_init the time when IRS started
#' @param coverage the coverage achieved
#' @param zap the coverage achieved
#' @return a **`ramp.xds`** model object
#' @export
add_irs_round = function(xds_obj, type, t_init, coverage, zap=1) {
  options <- list()
  options$type = c(xds_obj$irs$coverage_mod$type, type)
  options$t_init = c(xds_obj$irs$coverage_mod$t_init, t_init)
  options$coverage = c(xds_obj$irs$coverage_mod$coverage, coverage)
  options$zap = c(xds_obj$irs$coverage_mod$zap, zap)
  xds_obj$irs$coverage_mod = make_irs_multiround(options)
  return(xds_obj)
}

