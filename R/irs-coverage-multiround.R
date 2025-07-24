# Irs Multi-Round configures F_cover

#' @title IRS Coverage with Multiple Rounds
#' @description A model for IRS coverage over time
#' when there have been several, different IRS models
#' @inheritParams IRSCoverage
#' @return an **`xds`** object
#' @export
IRSCoverage.multiround <- function(t, pars) {with(pars$irs$coverage_mod,{
  pars$irs$coverage = F_cover(t)
  return(pars)
})}

#' @title Set up dynamic forcing
#'
#' @description A set up utility function for
#' [IRSCoverage].
#'
#' @inheritParams setup_irs_coverage
#'
#' @return an IRS coverage model object
#' @export
setup_irs_coverage.multiround = function(name, pars, opts=list()){
  setup_irs_multiround(opts)
}

#' @title Set up dynamic forcing
#'
#' @description Set up a function that computes
#' irs coverage over time.
#'
#' @param opts a list of options to override defaults
#' @param t_init the time when IRS started
#' @param coverage the coverage achieved
#' @param type the IRS type
#' @param zap the contact parameter
#'
#' @return an IRS coverage model object
#' @export
setup_irs_multiround = function(opts=list(),
                                t_init = 0,
                                coverage = 0,
                                type = "none",
                                zap = 1){
  with(opts,{
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
#' @return an **`xds`** object
#' @export
setup_F_cover_irs = function(cover){

  rounds <- list()
  rounds[[1]] = makepar_F_zero()
  for(i in 1:cover$nRounds)
    rounds[[i+1]] = with(cover,
                       setup_irs_round(type[i], t_init[i], coverage[i], zap[i]))

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
#' @param pars the `ramp.xds` object
#' @param type the name of the IRS type
#' @param t_init the time when IRS started
#' @param coverage the coverage achieved
#' @param zap the coverage achieved
#' @return an **`xds`** object
#' @export
add_irs_round = function(pars, type, t_init, coverage, zap=1) {
  opts <- list()
  opts$type = c(pars$irs$coverage_mod$type, type)
  opts$t_init = c(pars$irs$coverage_mod$t_init, t_init)
  opts$coverage = c(pars$irs$coverage_mod$coverage, coverage)
  opts$zap = c(pars$irs$coverage_mod$zap, zap)
  pars$irs$coverage_mod = setup_irs_multiround(opts)
  return(pars)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param type the name of the IRS type
#' @param t_init the time when IRS started
#' @param coverage the coverage achieved
#' @param zap contact scaling
#' @return an **`xds`** object
#' @export
setup_irs_round = function(type, t_init, coverage, zap=1) {
  class(type) <- type
  UseMethod("setup_irs_round", type)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param t_init the time when IRS started
#' @param uk scale up rate
#' @param L length of half-life
#' @param dk decay shape parameter
#' @param coverage the coverage achieved
#' @param zap contact scaling parameter
#' @return an **`xds`** object
#' @export
setup_irs_round_generic = function(t_init, uk=1/5, L=365, dk=1/60, coverage=.7, zap=1) {
  makepar_F_sharkfin(D=t_init, uk=uk, L=L, dk = dk, mx=coverage)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_irs_round
#' @return an **`xds`** object
#' @export
setup_irs_round.none = function(type, t_init, coverage, zap=1) {
  makepar_F_zero()
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_irs_round
#' @return an **`xds`** object
#' @export
setup_irs_round.actellic = function(type, t_init, coverage, zap=1) {
  makepar_F_sharkfin(D=t_init, uk=1/5, L=365, dk = 1/60, mx=coverage)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_irs_round
#' @return an **`xds`** object
#' @export
setup_irs_round.bendiocarb = function(type, t_init, coverage, zap=1) {
  makepar_F_sharkfin(D=t_init, uk=1/5, L=100, dk = 1/25, mx=coverage)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_irs_round
#' @return an **`xds`** object
#' @export
setup_irs_round.fludora_fusion = function(type, t_init, coverage, zap=1) {
  makepar_F_sharkfin(D=t_init, uk=1/5, L=310, dk = 1/35, mx=coverage)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_irs_round
#' @return an **`xds`** object
#' @export
setup_irs_round.sumishield = function(type, t_init, coverage, zap=1) {
  makepar_F_sharkfin(D=t_init, uk=1/5, L=365, dk = 1/75, mx=coverage)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_irs_round
#' @return an **`xds`** object
#' @export
setup_irs_round.pyrethroid = function(type, t_init, coverage, zap=1) {
  makepar_F_sharkfin(D=t_init, uk=1/5, L=180, dk = 1/100, mx=coverage)
}
