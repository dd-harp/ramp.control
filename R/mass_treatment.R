
#' Set Up Mass Treatment Events
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param jdates the julian dates of mass treatment events
#' @param span the treatment period (in days)
#' @param frac_tot the fraction treated
#' @param test FALSE for mass drug administration; TRUE for mass screen and treat
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
setup_mass_treat_events = function(xds_obj, jdates, span, frac_tot, test){
  N = length(jdates)
  stopifnot(length(span)==N)
  stopifnot(length(frac_tot)==N)
  stopifnot(length(test)==N)

  with(xds_obj,if(!exists("events_obj"))
     xds_obj$events_obj = list())

  xds_obj$events_obj$mass_treat = list()
  xds_obj$events_obj$mass_treat$N = N
  xds_obj$events_obj$mass_treat$jdate = jdates
  xds_obj$events_obj$mass_treat$span = span
  xds_obj$events_obj$mass_treat$frac_tot = frac_tot
  xds_obj$events_obj$mass_treat$test = test

  xds_obj <- setup_mass_treat_multiround(xds_obj)

  return(xds_obj)
}

#' Set Up Mass Treatment Events
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param jdates the julian dates of mass treatment events
#' @param span the treatment period
#' @param frac_tot the fraction treated
#' @param test FALSE for mass drug administration; TRUE for mass screen and treat
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
add_mass_treat_events = function(xds_obj, jdates, span, frac_tot, test){
  M = length(jdates)
  stopifnot(length(span)==M)
  stopifnot(length(frac_tot)==M)
  stopifnot(length(test)==M)

  with(xds_obj$events_obj,
    if(!exists("mass_treat"))
      return(setup_mass_treat_events(xds_obj, jdates, span, frac_tot, test)))

  new_jdates = c(xds_obj$events_obj$mass_treat$jdate, jdates)
  new_span = c(xds_obj$events_obj$mass_treat$span, span)
  new_frac_tot = c(xds_obj$events_obj$mass_treat$frac_tot,frac_tot)
  new_test = c(xds_obj$events_obj$mass_treat$test,test)

  xds_obj <- setup_mass_treat_events(xds_obj, new_jdates, new_span, new_frac_tot, new_test)

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
  if(length(mda_obj) > 0){
    xds_obj$mass_treat_obj$mda_obj <- mda_obj
    xds_obj$XH_obj[[1]]$mda = mda_obj$F_treat
  }

  msat_obj <- make_mass_treat_multiround(xds_obj, screen=TRUE)
  if(length(msat_obj) > 0){
    xds_obj$mass_treat_obj$msat_obj <- msat_obj
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

#' @title Show MDA
#' @description Plot the per-capita mass treatment rate
#'
#' @param tt a set of time points
#' @param xds_obj a **`ramp.xds`**  model object
#' @param clr the line color(s)
#' @param add if FALSE, use plot to draw new axes
#'
#' @return an **`xds`** model object
#'
#' @export
show_mda = function(tt, xds_obj, clr="black", add=FALSE){
  mda_t = xds_obj$mass_treat_obj$mda_obj$F_treat(tt)
  if(add==FALSE)
    graphics::plot(tt, mda_t, type = "n", xlab="Time (Days)", ylab = "Mass Treat Rate")
  graphics::lines(tt, mda_t, col=clr)
}

#' @title Show MSAT
#' @description Plot the per-capita mass treatment rate
#'
#' @param tt a set of time points
#' @param xds_obj a **`ramp.xds`**  model object
#' @param clr the line color(s)
#' @param add if FALSE, use plot to draw new axes
#'
#' @return an **`xds`** model object
#'
#' @export
show_msat = function(tt, xds_obj, clr="black", add=FALSE){
  msat_t = xds_obj$mass_treat_obj$msat_obj$F_treat(tt)
  if(add==FALSE)
    graphics::plot(tt, msat_t, type = "n", xlab="Time (Days)", ylab = "Mass Treat Rate")
  graphics::lines(tt, msat_t, col=clr)
}
