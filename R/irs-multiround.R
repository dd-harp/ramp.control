# irs-multiround configures F_trend

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param t_init the time when IRS started
#' @param coverage the coverage achieved
#' @param irs_type the IRS type
#' @param zap the contact parameter
#' @param opts a list of options to override defaults
#' @return an **`xds`** object
#' @export
setup_irs_multiround = function(t_init = 1, coverage=.8, irs_type = "actellic", zap = 1,
                                   opts=list()){
  with(opts,{
#    pars = dynamic_vector_control(pars)
#    coverage <- list()
#    class(coverage) <- 'multiround'
    irs_types <- unique(irs_type)
    stopifnot(length(t_init) == length(coverage))
    stopifnot(length(t_init) == length(irs_type))
    stopifnot(length(zap) == length(irs_types))
    rounds = setup_irs_round(irs_type[1], t_init[1], coverage[1], zap[1])
    for(i in 2:length(irs_type)){
      j = which(irs_types %in% irs_type[i])
      round = setup_irs_round(irs_type[i], t_init[i], coverage[i], zap[j])
      rounds = makepar_F_sum(rounds, round)
    }
    return(rounds)
})}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param name the name of the IRS type
#' @param t_init the time when IRS started
#' @param coverage the coverage achieved
#' @param zap the kill parameter
#' @return an **`xds`** object
#' @export
setup_irs_round = function(name, t_init, coverage, zap) {
  class(name) <- name
  UseMethod("setup_irs_round", name)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_irs_round
#' @return an **`xds`** object
#' @export
setup_irs_round.actellic = function(name, t_init, coverage, zap) {
  makepar_F_sharkfin(D=t_init, uk=1/5, L=365, dk = 1/60, mx=coverage*zap)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_irs_round
#' @return an **`xds`** object
#' @export
setup_irs_round.bendiocarb = function(name, t_init, coverage, zap) {
  makepar_F_sharkfin(D=t_init, uk=1/5, L=100, dk = 1/25, mx=coverage*zap)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_irs_round
#' @return an **`xds`** object
#' @export
setup_irs_round.fludora_fusion = function(name, t_init, coverage, zap) {
  makepar_F_sharkfin(D=t_init, uk=1/5, L=310, dk = 1/35, mx=coverage*zap)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_irs_round
#' @return an **`xds`** object
#' @export
setup_irs_round.sumishield = function(name, t_init, coverage, zap) {
  makepar_F_sharkfin(D=t_init, uk=1/5, L=365, dk = 1/75, mx=coverage*zap)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_irs_round
#' @return an **`xds`** object
#' @export
setup_irs_round.pyrethroid = function(name, t_init, coverage, zap) {
  makepar_F_sharkfin(D=t_init, uk=1/5, L=180, dk = 1/100, mx=coverage*zap)
}
