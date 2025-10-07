
#' Set Up Mass Treatment Events
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param jdates the julian dates of mass treatment events
#' @param span the treatment period
#' @param frac_treated the fraction treated
#' @param test FALSE for mass drug administration; TRUE for mass screen and treat
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
setup_mass_treat_events = function(xds_obj, jdates, span, frac_treated, test){
  N = length(jdates)
  stopifnot(length(span)==N)
  stopifnot(length(frac_treated)==N)
  stopifnot(length(test)==N)

  with(xds_obj,if(!exists("events_obj"))
     xds_obj$events_obj = list())

  xds_obj$events_obj$mass_treat = list()
  xds_obj$events_obj$mass_treat$N = N
  xds_obj$events_obj$mass_treat$jdate = jdates
  xds_obj$events_obj$mass_treat$span = span
  xds_obj$events_obj$mass_treat$frac_treated = frac_treated
  xds_obj$events_obj$mass_treat$test = test

  xds_obj <- setup_mass_treat_multiround(xds_obj)

  return(xds_obj)
}

#' @title Set Up a Bed Net Function
#'
#' @description
#' This sets up a function to set the value of bed net
#' treatage
#'
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @export
setup_mass_treat_multiround = function(xds_obj){

  with(xds_obj,stopifnot(exists("events_obj")))
  with(xds_obj$events_obj,stopifnot(exists("mass_treat")))

  xds_obj$mass_treat_obj = list()

  mda_obj <- make_mass_treat_multiround(xds_obj, screen=FALSE)
  xds_obj$mass_treat_obj$mda_obj <- mda_obj
  if(length(mda_obj) > 0)
    xds_obj$XH_obj[[1]]$mda = mda_obj$F_treat

  msat_obj <- make_mass_treat_multiround(xds_obj, screen=TRUE)
  xds_obj$mass_treat_obj$msat_obj <- msat_obj
  if(length(msat_obj) > 0)
    xds_obj$XH_obj[[1]]$msat = msat_obj$F_treat

  return(xds_obj)
}

#' @title Make Multiple Rounds of msat
#'
#' @description
#'
#' Using information about the events (see [setup_mass_treat_events]),
#' including a parameter describing access, this makes a
#' function that computes treatage over time.
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param screen a switch
#'
#' @return a **`xds`** object
#' @export
make_mass_treat_multiround = function(xds_obj, screen){
  with(xds_obj, stopifnot(exists("events_obj")))
  with(xds_obj$events_obj, stopifnot(exists("mass_treat")))
  with(xds_obj$events_obj$mass_treat,{
    ix = which(test==screen)
    nRounds = length(ix)
    treat = list()
    if(nRounds > 0){
      treat$nRounds = nRounds
      treat$t_init  = jdate[ix]
      treat$span    = span[ix]
      treat$frac_treated = frac_treated[ix]
      treat = make_F_mass_treat(treat)
    }
    return(treat)
})}


#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param treat a list with parameters for all the rounds
#' @return a **`xds`** object
#' @export
make_F_mass_treat = function(treat){with(treat,{
  rounds <- list()
  for(i in 1:treat$nRounds){
    rate = -log(1-frac_treated)/span[i]
    pars <- makepar_F_sharkfin(D=t_init[i], L=span[i], uk=3, dk=3, mx=rate)
    rounds[[i]] = pars
    rounds_par <- makepar_F_multiround(treat$nRounds, rounds)
    treat$rounds <- rounds
    treat$rounds_par <- rounds_par
    treat$F_treat <- make_function(rounds_par)
  }
  return(treat)
})}
