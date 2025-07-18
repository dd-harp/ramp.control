
#' @title Set no distribute_bednets
#' @description The null model for distribute_bednets
#' @inheritParams DistributeBedNets
#' @return [list]
#' @export
DistributeBedNets.func <- function(t, pars) {
  #F_distribute_bednets(t, pars)
  return(pars)
}

#' @title Set up dynamic bednets
#' @description If dynamic bednets has not
#' already been set up, then turn on dynamic
#' bednets and set all the
#' @inheritParams setup_distribute_bednets
#' @export
setup_distribute_bednets.func = function(name="func", pars, opts=list()){
  pars <- setup_distribute_bednets_func(pars, opts)
}

#' @title Set up dynamic bednets
#' @description If dynamic bednets has not
#' already been set up, then turn on dynamic
#' bed net  and set all the
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @param eventT the time when a distribute occurs
#' @param F_distribute the effects of the distribute
#' @return a bed net distribution model object
#' @export
setup_distribute_bednets_func = function(pars, opts=list(), eventT=365, F_distribute=NULL){
  distribute <- list()
  class(distribute) <- 'func'
  distribute$eventT = eventT
  distribute$F_distribute = F_distribute
  return(distribute)
}

#' @title Set no own_bednets
#' @description The null model for own_bednets
#' @inheritParams OwnBedNets
#' @return [list]
#' @export
OwnBedNets.func <- function(t, pars) {with(pars$bednets$owner_mod,{
  pars$bednets$own = mean*F_season(t)*F_trend(t)
})}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_own_bednets
#' @export
setup_own_bednets.func = function(name, pars, opts=list()){
  pars = setup_own_bednets_func(pars, opts)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @param mean mean bednet ownership
#' @param F_season the seasonal signal in bednet ownership
#' @param F_trend a temporal trend in bednet ownership
#' @return an **`xds`** object
#' @export
setup_own_bednets_func = function(pars, opts=list(), mean=0.7, F_season=F_flat, F_trend=F_flat){
  own <- list()
  class(own) <- 'func'
  own$mean <- mean
  own$F_season <- F_season
  own$F_trend <- F_trend
  return(own)
}

#' @title Set no use_bednets
#' @description The null model for use_bednets
#' @inheritParams UseBedNets
#' @return [list]
#' @export
UseBedNets.func <- function(t, pars) {with(pars$bednets$user_mod,{
  pars$bednets$use = mean*F_season(t)*F_trend(t)
})}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_use_bednets
#' @export
setup_use_bednets.func = function(name, pars, opts=list()){
  pars = setup_use_bednets_func(pars, opts)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @param mean mean bednet useership
#' @param F_season the seasonal signal in bednet useership
#' @param F_trend a temporal trend in bednet useership
#' @return an **`xds`** object
#' @export
setup_use_bednets_func = function(pars, opts=list(), mean=0.7, F_season=F_flat, F_trend=F_flat){
  use <- list()
  class(use) <- 'func'

  use$mean <- mean
  use$F_season <- F_season
  use$F_trend <- F_trend
  return(use)
}

#' @title Set no bednet_coverage
#' @description The null model for bednet_coverage
#' @inheritParams BedNetCoverage
#' @return [list]
#' @export
BedNetCoverage.func <- function(t, pars) {with(pars$bednets$coverage_mod,{
  pars$bednets$coverage = pmin(pmax(0, mean*F_season(t)*F_trend(t)),1)
  return(pars)
})}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_bednet_coverage
#' @export
setup_bednet_coverage.func = function(name, pars, opts=list()){
  setup_bednet_coverage_func(pars, opts)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @param mean the mean bednet_coverage
#' @param F_season the seasonal signal in bednet coverage
#' @param season_par parameters to configure F_season
#' @param F_trend a temporal trend in bednet coverage
#' @param trend_par parameters to configure F_trend
#' @return an **`xds`** object
#' @export
setup_bednet_coverage_func = function(pars, opts=list(),
                                      mean=1,
                                      F_season=F_flat,
                                      season_par=list(),
                                      F_trend=F_flat,
                                      trend_par=list()){
  with(opts,{
    pars = dynamic_vector_control(pars)
    coverage <- list()
    class(coverage) <- 'func'
    coverage$name <- 'func'
    coverage$mean <- mean
    coverage$F_season <- F_season
    if(length(season_par)>1){
      coverage$season_par <- season_par
      coverage$F_season <- make_function(season_par)
    }
    coverage$F_trend <- F_trend
    if(length(trend_par)>1){
      coverage$trend_par <- trend_par
      coverage$F_trend <- make_function(trend_par)
    }
    return(coverage)
})}

#' @title Set no bednet
#' @description The null model for bednet
#' @inheritParams BedNetEffectSizes
#' @return [list]
#' @export
BedNetEffectSizes.func <- function(t, pars, s) {
  ## set es_f
  ## set es_q
  ## set es_g
  return(pars)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_bednet_effect_sizes
#' @return an **`xds`** object
#' @export
setup_bednet_effect_sizes.func = function(name, pars, opts=list()){
  setup_bednet_effect_sizes_func(pars, opts)
}

#' @title Set no bednet
#' @description The null model for bednet
#' @param pars an **`xds`** object
#' @param opts an optional list of arguments
#' @return a bednet effect size model object
#' @export
setup_bednet_effect_sizes_func <- function(pars, opts) {
  coverage <- list()
  class(coverage) <- 'func'
  ## setup function to compute es_f
  ## setup function to compute es_q
  ## setup function to compute es_g
  return(coverage)
}
