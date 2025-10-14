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
#' @param jdates the julian dates of bed net mass distribution events
#' @param net_type the type of net used
#' @param peak_access the fraction of the population with access to a bed net
#' @param contact the contact parameter
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
setup_bednet_events = function(xds_obj, jdates, net_type, peak_access, contact=1){

  xds_obj <- setup_vector_control(xds_obj)

  N = length(jdates)
  stopifnot(length(net_type)==N)
  stopifnot(length(peak_access)==N)

  with(xds_obj,if(!exists("events_obj"))
    xds_obj$events_obj = list())

  xds_obj$events_obj$bednet = list()
  xds_obj$events_obj$bednet$N = N
  xds_obj$events_obj$bednet$jdate = jdates
  xds_obj$events_obj$bednet$type  = net_type
  xds_obj$events_obj$bednet$peak  = peak_access
  xds_obj$events_obj$bednet$contact = checkIt(contact, N)

  xds_obj <- setup_bednet_coverage("multiround", xds_obj)
  xds_obj <- setup_bednet_contact("multiround", xds_obj)

  return(xds_obj)
}


#' @title Add Bed Net Rounds
#'
#' @description Add one or more bed net
#' rounds to the the events list
#'
#' @param xds_obj the `ramp.xds` object
#' @param jdates the julian date of the bed net mass distribution round
#' @param net_type the type of net used
#' @param peak_access the fraction of the population with access to a bed net
#'
#' @return a **`xds`** object
#' @export
add_bednet_events = function(xds_obj, jdates, net_type, peak_access) {
  M = length(jdates)
  stopifnot(length(net_type)==M)
  stopifnot(length(peak_access)==M)

  with(xds_obj$events_obj,
       if(!exists("bednet"))
         return(setup_bednet_events(xds_obj, jdates, net_type, peak_access)))

  with(xds_obj$bednet_obj, stopifnot(exists("events")))

  new_jdates = c(xds_obj$events_obj$bednet$jdate, jdates)
  new_net_type = c(xds_obj$events_obj$bednet$net_type, net_type)
  new_peak_access = c(xds_obj$events_obj$bednet$peak_access,peak_access)

  xds_obj <- setup_bednet_events(xds_obj, new_jdates, new_net_type, new_peak_access)


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
            points(peak[i]*mx, jdate[i], pch = 19, col = clr)
            segments(jdate[i], mn, jdate[i], mx, col = clr)
            label = paste(i, "-", type[i])
            text(jdate[i], .1*mx, label, pos=4, srt=90, col = clr)
}}})}

#' Set Up IRS Evaluation
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param jdates the julian dates of IRS events
#' @param pesticides the pesticide used
#' @param frac_sprayed the fraction of houses sprayed
#' @param contact the contact parameter
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
setup_irs_events = function(xds_obj, jdates, pesticides, frac_sprayed, contact=1){

  xds_obj <- setup_vector_control(xds_obj)

  N = length(jdates)
  stopifnot(length(pesticides)==N)
  stopifnot(length(frac_sprayed)==N)

  xds_obj$events_obj$irs = list()
  xds_obj$events_obj$irs$N = N
  xds_obj$events_obj$irs$jdate = jdates
  xds_obj$events_obj$irs$type  = pesticides
  xds_obj$events_obj$irs$peak  = frac_sprayed
  xds_obj$events_obj$irs$contact = checkIt(contact, N)

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
#' @param jdate the julian date of the bed net mass distribution round
#' @param net_type the type of net used
#' @param peak the fraction of houses sprayed
#'
#' @return a **`xds`** object
#' @export
add_irs_events = function(xds_obj, jdate, net_type, peak) {
  with(xds_obj$irs_obj, stopifnot(exists("events")))
  N_new = length(jdate)
  stopifnot(length(net_type) == N_new)
  stopifnot(length(peak) == N_new)

  xds_obj$events_obj$irs$N = xds_obj$events_obj$irs$N + N_new
  xds_obj$events_obj$irs$jdate = c(xds_obj$events_obj$irs$jdate, jdate)
  xds_obj$events_obj$irs$net_type = c(xds_obj$events_obj$irs$net_type, net_type)
  xds_obj$events_obj$irs$peak = c(xds_obj$events_obj$irs$peak, peak)

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
          if(jdate[i]>0){
            points(mx, jdate[i])
            segments(jdate[i], mn, jdate[i], mx, col = clr)
            label = paste(i, "-", type[i])
            text(jdate[i], .8*mx, label, pos=2, srt=90, col = clr)
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
