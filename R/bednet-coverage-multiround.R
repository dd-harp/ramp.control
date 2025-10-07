# Bed Net multiround


#' Get Bednet Contact Parameter
#'
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @returns the contact parameter
#'
#' @export
get_bednet_contact = function(xds_obj){
  xds_obj$bednet_obj$cover_obj$contact
}

#' Set Up Bed Net Evaluation
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param jdates the julian dates of bed net mass distribution events
#' @param net_type the type of net used
#' @param peak_access the fraction of the population with access to a bed net
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
setup_bednet_events = function(xds_obj, jdates, net_type, peak_access){
  N = length(jdates)
  stopifnot(length(net_type)==N)
  stopifnot(length(peak_access)==N)

  xds_obj$bednet_obj$events = list()
  xds_obj$bednet_obj$events$N = N
  xds_obj$bednet_obj$events$jdate = jdates
  xds_obj$bednet_obj$events$type  = net_type
  xds_obj$bednet_obj$events$peak  = peak_access
  xds_obj$bednet_obj$events$contact = rep(1, N)

  return(xds_obj)
}

#' Show bednet Events
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param mn the bottom of the segment
#' @param mx the top of the segment
#' @param clr a color for the line segments
#'
#' @importFrom graphics points text segments
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
show_bednet_events = function(xds_obj, mn=0, mx=1, clr="#E4460AFF"){
  with(xds_obj$bednet_obj$events,{
    for(i in 1:N){
      if(jdate[i]>0){
        points(peak[i]*mx, jdate[i], pch = 19, col = clr)
        segments(jdate[i], mn, jdate[i], mx, col = clr)
        label = paste(i, "-", type[i])
        text(jdate[i], .1*mx, label, pos=4, srt=90, col = clr)
      }
    }
  })
}


#' @title Set Up a Bed Net Coverage Function
#'
#' @description
#' This sets up a function to set the value of bed net
#' coverage
#'
#' @inheritParams setup_bednet_coverage
#' @export
setup_bednet_coverage.multiround = function(name="multiround", xds_obj, options=list()){
  class(xds_obj$vector_control_obj) = "dynamic"
  class(xds_obj$bednet_obj) = "dynamic"
  xds_obj$bednet_obj$cover_obj <- make_bednet_multiround(xds_obj, options)
  return(xds_obj)
}

#' @title Set no bednet_coverage
#' @description The null model for bednet_coverage
#' @inheritParams Bed_Net_Coverage
#' @return a **`xds`** object
#' @export
Bed_Net_Coverage.multiround <- function(t, y, xds_obj) {
  with(xds_obj$bednet_obj$cover_obj,{
    xds_obj$bednet_obj$coverage = F_cover(t)
    return(xds_obj)
})}

#' @title Make the Multiround Bednet Coverage Object
#'
#' @description
#'
#' Using information about the events (see [setup_bednet_events]),
#' including a parameter describing access, this makes a
#' function that computes coverage over time.
#'
#'
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param options set up options to override defaults
#'
#' @return a **`xds`** object
#' @export
make_bednet_multiround = function(xds_obj, options = list()){
  with(xds_obj$bednet_obj, stopifnot(exists("events")))
  with(xds_obj$bednet_obj$events,{
    stopifnot(N>0)
    with(options,{
      if(!exists("include")) include = rep(TRUE, N)
      nRounds <- sum(include)
      cover <- list()
      class(cover) <- "multiround"
      cover$nRounds = nRounds
      cover$t_init  = jdate[include]
      cover$peak      = peak[include]
      cover$type    = type[include]
      cover$contact = contact[include]
      cover = make_F_cover_bednet(cover)

      return(cover)
})})}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param cover a list with parameters for all the rounds
#' @return a **`xds`** object
#' @export
make_F_cover_bednet = function(cover){

  rounds <- list()
  for(i in 1:cover$nRounds)
    rounds[[i]] = with(cover, make_bednet_round(type[i], t_init[i], peak[i], contact[i]))

  rounds_par <- makepar_F_multiround(cover$nRounds, rounds)
  cover$rounds <- rounds
  cover$rounds_par <- rounds_par
  cover$F_cover <- make_function(rounds_par)

  return(cover)
}


#' Set values for bednet coverage
#'
#' @param contact contact parameter(s)
#' @param xds_obj  a **`ramp.xds`** model object
#'
#' @returns a **`ramp.xds`** model object
#'
#' @export
change_bednet_contact_multiround = function(contact, xds_obj){
  xds_obj$bednet_obj$cover_obj$contact = contact
  xds_obj$bednet_obj$cover_obj <-  make_F_cover_bednet(xds_obj$bednet_obj$cover_obj)
  return(xds_obj)
}


#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param xds_obj the `ramp.xds` object
#' @param jdate the julian date of the bed net mass distribution round
#' @param net_type the type of net used
#' @param peak_access the fraction of the population with access to a bed net
#'
#' @return a **`xds`** object
#' @export
add_bednet_round = function(xds_obj, jdate, net_type, peak_access) {
  with(xds_obj$bednet_obj, stopifnot(exists("events")))
  N_new = length(jdate)
  stopifnot(length(net_type) == N_new)
  stopifnot(length(peak_access) == N_new)
  xds_obj$bednet_obj$events$N = xds_obj$bednet_obj$events$N + N_new
  xds_obj$bednet_obj$events$jdate = c(xds_obj$bednet_obj$events$jdate, jdate)
  xds_obj$bednet_obj$events$net_type = c(xds_obj$bednet_obj$events$net_type, net_type)
  xds_obj$bednet_obj$events$peak_access = c(xds_obj$bednet_obj$events$peak_access, peak_access)
  xds_obj$bednet_obj$events$contact = c(xds_obj$bednet_obj$events$contact, rep(1, N_new))
  return(xds_obj)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param type the name of the BedNet type
#' @param t_init the time when BedNet started
#' @param peak the peack access achieved
#' @param contact contact scaling
#' @return a **`xds`** object
#' @export
make_bednet_round = function(type, t_init, peak, contact=1) {
  class(type) <- type
  UseMethod("make_bednet_round", type)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param t_init the time when BedNet started
#' @param uk scale up rate
#' @param L length of half-life
#' @param dk decay shape parameter
#' @param peak the peak access achieved
#' @param contact contact scaling parameter
#' @return a **`xds`** object
#' @export
make_bednet_round_generic = function(t_init, uk=1/5, L=365, dk=1/60, peak=.7, contact=1) {
  makepar_F_sharkfin(D=t_init, uk=uk, L=L, dk = dk, mx=peak*contact)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams make_bednet_round
#' @return a **`xds`** object
#' @export
make_bednet_round.pbo = function(type, t_init, peak, contact=1) {
  makepar_F_sharkfin(D=t_init, uk=1/5, L=365, dk = 1/60, mx=peak*contact)
}
