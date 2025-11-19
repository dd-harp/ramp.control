
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
#' @param ypos the position of the interpolation points
#' @param bny the position of bednet labels
#' @param irsy the position of irs labels
#' @param bclr a color for the bednet line segments
#' @param iclr a color for the irs line segments
#' @param add if TRUE, add to an existing plot
#'
#' @importFrom graphics points text
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
show_events = function(xds_obj, mn=0, mx=1, ypos=0, bny=0, irsy=1, bclr="#E4460AFF", iclr = "#4686FBFF", add=FALSE){
  show_bednet_events(xds_obj, mn, mx, bny, bclr)
  show_irs_events(xds_obj, mn, mx, irsy, iclr, add=TRUE)
  xds_obj$data$tt -> tt
  xds_obj$data$yy -> yy
  points(tt, yy*0+ypos, pch=10, cex=1.5, col ="#30123BFF")
  text(tt, yy*0+ypos, 1:length(tt), pos=3, col = "#30123BFF")
}


#' Show IRS Events
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param mn the bottom of the segment
#' @param mx the top of the segment
#' @param laby the height of the labels
#' @param clr a color for the line segments
#' @param add if TRUE, add to an existing plot
#'
#' @importFrom graphics points text segments
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
show_irs_events = function(xds_obj, mn=0, mx=1, laby = 1, clr="#4686FBFF", add=FALSE){
  if(with(xds_obj, exists("events_obj")))
    if(with(xds_obj$events_obj, exists("irs")))
      with(xds_obj$events_obj$irs,{
        for(i in 1:N){
          segments(start_day[i], mn, start_day[i], mx, col = clr)
          #          label = paste(i, "-", type[i])
          #          text(start_day[i], .8*mx, label, pos=2, srt=90, col = clr)
          ypos <- mn + (mx-mn)*laby
          points(start_day[i], ypos, pch = 21, col = "white", bg="white", cex=3)
          text(start_day[i], ypos, i, col = clr)
        }})}

#' Show bednet Events
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param mn the bottom of the segment
#' @param mx the top of the segment
#' @param laby the height of the labels
#' @param clr a color for the line segments
#' @param add if TRUE, add to an existing plot
#'
#' @importFrom graphics points text segments
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
show_bednet_events = function(xds_obj, mn=0, mx=1, laby=0.1, clr="#E4460AFF", add=FALSE){
  if(with(xds_obj, exists("events_obj")))
    if(with(xds_obj$events_obj, exists("bednet")))
      with(xds_obj$events_obj$bednet,{
        for(i in 1:N){
          segments(start_day[i], mn, start_day[i], mx, col = clr)
          #          label = paste(i, "-", net_type[i])
          #          points(peak_access[i]*mx, start_day[i], pch = 19, col = clr)
          ypos <- mn + (mx-mn)*laby
          points(start_day[i], ypos, pch = 21, col = "white", bg="white", cex=3)
          text(start_day[i], ypos, i, col = clr)
        }})}


#' Show IRS Events
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param mn the bottom of the segment
#' @param mx the top of the segment
#' @param laby the height of the labels
#' @param clr a color for the line segments
#' @param add if TRUE, add to an existing plot
#'
#' @importFrom graphics points text segments
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
show_irs_events = function(xds_obj, mn=0, mx=1, laby = 1, clr="#4686FBFF", add=FALSE){
  if(with(xds_obj, exists("events_obj")))
    if(with(xds_obj$events_obj, exists("irs")))
      with(xds_obj$events_obj$irs,{
        for(i in 1:N){
          segments(start_day[i], mn, start_day[i], mx, col = clr)
#          label = paste(i, "-", type[i])
#          text(start_day[i], .8*mx, label, pos=2, srt=90, col = clr)
          ypos <- mn + (mx-mn)*laby
          points(start_day[i], ypos, pch = 21, col = "white", bg="white", cex=3)
          text(start_day[i], ypos, i, col = clr)
}})}


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

  if(with(xds_obj,!exists("events_obj")))
    xds_obj$events_obj = list()

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
