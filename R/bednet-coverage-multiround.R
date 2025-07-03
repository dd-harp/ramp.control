# Bed Net multiround

#' @title Set no bednet_coverage
#' @description The null model for bednet_coverage
#' @inheritParams BedNetCoverage
#' @return an **`xds`** object
#' @export
BedNetCoverage.multiround <- function(t, pars) {with(pars$bednet$coverage_mod,{
  pars$bednet$coverage = F_cover(t)
  return(pars)
})}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_bednet_coverage
#' @return an **`xds`** object
#' @export
setup_bednet_coverage.multiround = function(bednet_type, pars, opts=list()){
  setup_bednet_multiround(opts)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param opts a list of options to override defaults
#' @param t_init the time when BedNet started
#' @param coverage the coverage achieved
#' @param bednet_type the BedNet type
#' @param zap the contact parameter
#' @return an **`xds`** object
#' @export
setup_bednet_multiround = function(opts=list(),
                                t_init = 1,
                                coverage=.8,
                                bednet_type = "PBO",
                                zap = 1){
  with(opts,{
    cover <- list()
    class(cover) <- "multiround"
    cover$t_init = t_init
    cover$coverage = coverage
    cover$bednet_type = bednet_type
    cover$zap = zap

    nRounds <- length(t_init)
    cover$nRounds <- nRounds
    stopifnot(length(coverage) == nRounds)
    stopifnot(length(bednet_type) == nRounds)

    rounds <- list()
    for(i in 1:nRounds)
      rounds[[i]] = setup_bednet_round(bednet_type[i], t_init[i], coverage[i], zap[i])

    rounds_par <- makepar_F_multiround(nRounds, rounds)
    cover$rounds <- rounds
    cover$rounds_par <- rounds_par
    cover$F_cover <- make_function(rounds_par)

    return(cover)
  })}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param pars the `ramp.xds` object
#' @param bednet_type the name of the BedNet type
#' @param t_init the time when BedNet started
#' @param coverage the coverage achieved
#' @param zap the coverage achieved
#' @return an **`xds`** object
#' @export
add_bednet_round = function(pars, bednet_type, t_init, coverage, zap=1) {
  opts <- list()
  opts$bednet_type = c(pars$bednet$coverage_mod$bednet_type, bednet_type)
  opts$t_init = c(pars$bednet$coverage_mod$t_init, t_init)
  opts$coverage = c(pars$bednet$coverage_mod$coverage, coverage)
  opts$zap = c(pars$bednet$coverage_mod$zap, zap)
  pars$bednet$coverage_mod = setup_bednet_multiround(opts)
  return(pars)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param bednet_type the name of the BedNet type
#' @param t_init the time when BedNet started
#' @param coverage the coverage achieved
#' @param zap contact scaling
#' @return an **`xds`** object
#' @export
setup_bednet_round = function(bednet_type, t_init, coverage, zap=1) {
  class(bednet_type) <- bednet_type
  UseMethod("setup_bednet_round", bednet_type)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param t_init the time when BedNet started
#' @param uk scale up rate
#' @param L length of half-life
#' @param dk decay shape parameter
#' @param coverage the coverage achieved
#' @param zap contact scaling parameter
#' @return an **`xds`** object
#' @export
setup_bednet_round_generic = function(t_init, uk=1/5, L=365, dk=1/60, coverage=.7, zap=1) {
  makepar_F_sharkfin(D=t_init, uk=uk, L=L, dk = dk, mx=coverage^zap)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_bednet_round
#' @return an **`xds`** object
#' @export
setup_bednet_round.pbo = function(bednet_type, t_init, coverage, zap=1) {
  makepar_F_sharkfin(D=t_init, uk=1/5, L=365, dk = 1/60, mx=coverage^zap)
}

