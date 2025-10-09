

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

  mda_obj <- make_mass_treat_multiround(xds_obj, screen=FALSE)
  if(length(mda_obj) > 0){
    xds_obj$mda_obj <- mda_obj
    xds_obj$XH_obj[[1]]$mda = mda_obj$F_treat
  }

  msat_obj <- make_mass_treat_multiround(xds_obj, screen=TRUE)
  if(length(msat_obj) > 0){
    xds_obj$msat_obj <- msat_obj
    xds_obj$XH_obj[[1]]$msat = msat_obj$F_treat
  }

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
      treat$frac_tot = frac_tot[ix]
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
    rate = -log(1-frac_tot[i])/span[i]
    pars <- makepar_F_sharkfin(D=t_init[i], L=span[i], uk=3, dk=3, mx=rate)
    rounds[[i]] = pars
  }
  rounds_par <- makepar_F_multiround(treat$nRounds, rounds)
  treat$rounds <- rounds
  treat$rounds_par <- rounds_par
  treat$F_treat <- make_function(rounds_par)
  return(treat)
})}

