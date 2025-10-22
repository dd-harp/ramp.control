
#' Get Events
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param type the event type
#'
#' @returns Events details as a list
#'
#' @export
get_events = function(xds_obj, type){
  class(type) = type
  UseMethod("get_events", type)
}

#' Get IRS Events
#'
#' @inheritParams get_events
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
get_events.irs = function(xds_obj, type){
  xds_obj$events_obj$irs
}

#' Get Bednet Events
#'
#' @inheritParams get_events
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
get_events.bednet = function(xds_obj, type){
  xds_obj$events_obj$bednet
}

#' Show Events
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param mn the bottom of the segment
#' @param mx the top of the segment
#' @param bclr a color for the bednet line segments
#' @param iclr a color for the irs line segments
#' @param add if TRUE, add to an existing plot
#'
#' @importFrom graphics points text
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
show_events = function(xds_obj, mn=0, mx=1, bclr="#E4460AFF", iclr = "#4686FBFF", add=FALSE){
  show_bednet_events(xds_obj, mn, mx, bclr)
  show_irs_events(xds_obj, mn, mx, iclr, add=TRUE)
  xds_obj$data$tt -> tt
  xds_obj$data$yy -> yy
  points(tt, yy*0, pch=10, cex=1.5, col ="#30123BFF")
  text(tt, yy*0, 1:length(tt), pos=3, col = "#30123BFF")
}


#' Set Up Bed Net Evaluation
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param start_day the julian dates of bed net mass distribution events
#' @param net_type the type of net used
#' @param coverage_profile the type of net used
#' @param peak_access the fraction of the population with access to a bed net
#' @param event_length the length of the bed net distribution event
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
setup_bednet_events = function(xds_obj, start_day, net_type, coverage_profile, peak_access, event_length=20){

  xds_obj <- setup_vector_control(xds_obj)

  N = length(start_day)
  stopifnot(length(net_type)==N)
  stopifnot(length(coverage_profile)==N)
  stopifnot(length(peak_access)==N)

  with(xds_obj,if(!exists("events_obj"))
    xds_obj$events_obj = list())

  xds_obj$events_obj$bednet = list()
  xds_obj$events_obj$bednet$N = N
  xds_obj$events_obj$bednet$start_day = start_day
  xds_obj$events_obj$bednet$net_type  = net_type
  xds_obj$events_obj$bednet$coverage_profile  = coverage_profile
  xds_obj$events_obj$bednet$peak_access = peak_access
  xds_obj$events_obj$bednet$event_length = length

  xds_obj <- setup_bednet_coverage("multiround", xds_obj)
  xds_obj <- setup_bednet_contact("multiround", xds_obj)

  return(xds_obj)
}


#' @title Add Bed Net Rounds
#'
#' @description Add one or more bed net
#' rounds to the the events list
#'
#' @inheritParams setup_bednet_events
#'
#' @return a **`xds`** object
#' @export
add_bednet_events = function(xds_obj, start_day, net_type, coverage_profile, peak_access, event_length=20) {
  M = length(start_day)
  stopifnot(length(net_type)==M)
  stopifnot(length(peak_access)==M)
  stopifnot(length(coverage_profile)==M)
  event_length = checkIt(event_length, M)

  if(with(xds_obj$events_obj, !exists("bednet")))
         return(setup_bednet_events(xds_obj, start_day, net_type, coverage_profile, peak_access))

  new_start_day = c(xds_obj$events_obj$bednet$start_day, start_day)
  new_net_type = c(xds_obj$events_obj$bednet$net_type, net_type)
  new_coverage_profile = c(xds_obj$events_obj$bednet$coverage_profile, coverage_profile)
  new_peak_access = c(xds_obj$events_obj$bednet$peak_access, peak_access)
  new_event_length = c(xds_obj$events_obj$bednet$event_length, event_length)

  xds_obj <- setup_bednet_events(xds_obj, new_start_day, new_net_type, new_coverage_profile, new_peak_access, new_event_length)

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
  if(with(xds_obj, exists("events_obj")))
    if(with(xds_obj$events_obj, exists("bednet")))
      with(xds_obj$events_obj$bednet,{
        for(i in 1:N){
          if(jdate[i]>0){
            points(peak_access[i]*mx, start_day[i], pch = 19, col = clr)
            segments(start_day[i], mn, start_day[i], mx, col = clr)
            label = paste(i, "-", type[i])
            text(start_day[i], 0.4*mx, label, pos=4, srt=90, col = clr)
}}})}

#' Set Up IRS Evaluation
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param jdates the julian dates of IRS events
#' @param pesticides the pesticide used
#' @param frac_sprayed the fraction of houses sprayed
#' @param event_length the length of the distribution event
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
setup_irs_events = function(xds_obj, jdates, pesticides, frac_sprayed, event_length=20){

  xds_obj <- setup_vector_control(xds_obj)

  N = length(jdates)
  stopifnot(length(pesticides)==N)
  stopifnot(length(frac_sprayed)==N)
  length=checkIt(length, N)

  xds_obj$events_obj$irs = list()
  xds_obj$events_obj$irs$N = N
  xds_obj$events_obj$irs$irs_type  = pesticides
  xds_obj$events_obj$irs$start_day = jdates
  xds_obj$events_obj$irs$frac_sprayed = frac_sprayed
  xds_obj$events_obj$irs$event_length = event_length

  xds_obj <- setup_irs_coverage("multiround", xds_obj)
  xds_obj <- setup_irs_contact("multiround", xds_obj)

  return(xds_obj)
}

#' @title Add IRS rounds
#'
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param xds_obj the `ramp.xds` object
#' @param jdates the julian dates of IRS events
#' @param pesticides the pesticide used
#' @param frac_sprayed the fraction of houses sprayed
#' @param contact the contact parameter
#'
#' @return a **`xds`** object
#' @export
add_irs_events = function(xds_obj, jdates, pesticides, frac_sprayed, contact) {
  M = length(jdates)
  stopifnot(length(pesticides)==M)
  stopifnot(length(frac_sprayed)==M)
  stopifnot(length(contact)==M)

  if(with(xds_obj$events_obj, !exists("irs")))
         return(setup_irs_events(xds_obj, jdates, pesticides, frac_sprayed, contact))

  new_jdates = c(xds_obj$events_obj$irs$jdate, jdates)
  new_type = c(xds_obj$events_obj$irs$type, pesticides)
  new_frac_sprayed = c(xds_obj$events_obj$irs$frac_sprayed, frac_sprayed)
  new_contact = c(xds_obj$events_obj$irs$contact, contact)

  xds_obj <- setup_irs_events(xds_obj, new_jdates, new_type, new_frac_sprayed, new_contact)
  return(xds_obj)
}


#' Show IRS Events
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param mn the bottom of the segment
#' @param mx the top of the segment
#' @param clr a color for the line segments
#' @param add if TRUE, add to an existing plot
#'
#' @importFrom graphics points text segments
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
show_irs_events = function(xds_obj, mn=0, mx=1, clr="#4686FBFF", add=FALSE){
  if(with(xds_obj, exists("events_obj")))
    if(with(xds_obj$events_obj, exists("irs")))
      with(xds_obj$events_obj$irs,{
        for(i in 1:N){
          if(start_day[i]>0){
            points(mx, start_day[i])
            segments(start_day[i], mn, start_day[i], mx, col = clr)
            label = paste(i, "-", type[i])
            text(start_day[i], .8*mx, label, pos=2, srt=90, col = clr)
}}})}


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
  mda_t = xds_obj$mda_obj$F_treat(tt)
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
  msat_t = xds_obj$msat_obj$F_treat(tt)
  if(add==FALSE)
    graphics::plot(tt, msat_t, type = "n", xlab="Time (Days)", ylab = "Mass Treat Rate")
  graphics::lines(tt, msat_t, col=clr)
}
