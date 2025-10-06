# Irs Multi-Round configures F_cover


#' Set Up IRS Evaluation
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param jdates the julian dates of IRS events
#' @param pesticides the pesticide used
#' @param frac_sprayed the fraction of houses sprayed
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
setup_irs_events = function(xds_obj, jdates, pesticides, frac_sprayed){
  N = length(jdates)
  stopifnot(length(pesticides)==N)
  stopifnot(length(frac_sprayed)==N)

  xds_obj$irs_obj$events = list()
  xds_obj$irs_obj$events$N = N
  xds_obj$irs_obj$events$jdate = jdates
  xds_obj$irs_obj$events$type  = pesticides
  xds_obj$irs_obj$events$peak  = frac_sprayed
  xds_obj$irs_obj$events$round = rep(FALSE, N)

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
  if(add==FALSE) show_fit(xds_obj)
  with(xds_obj$irs_obj$events,{
    for(i in 1:N){
      if(jdate[i]>0){
        points(mx, jdate[i])
        segments(jdate[i], mn, jdate[i], mx, col = clr)
        label = paste(i, "-", type[i])
        text(jdate[i], .8*mx, label, pos=2, srt=90, col = clr)
      }
    }
  })
}


#' @title IRS Coverage with Multiple Rounds
#' @description A model for IRS coverage over time
#' when there have been several, different IRS models
#' @inheritParams IRS_Coverage
#' @return a **`ramp.xds`** model object
#' @export
IRS_Coverage.multiround <- function(t, y, xds_obj) {
  with(xds_obj$irs_obj$cover_obj,{
    xds_obj$irs_obj$coverage = F_cover(t)
    return(xds_obj)
})}

#' @title Set up dynamic forcing
#'
#' @description A set up utility function for
#' [IRS_Coverage].
#'
#' @inheritParams setup_irs_coverage
#'
#' @return a IRS coverage model object
#' @export
setup_irs_coverage.multiround = function(name, xds_obj, options=list()){
  xds_obj$irs_obj
  xds_obj$irs_obj$cover_obj <- make_irs_multiround(options)
  return(xds_obj)
}

#' Set values for irs coverage
#'
#' @param coverage coverage parameters
#' @param xds_obj  a **`ramp.xds`** model object
#'
#' @returns a **`ramp.xds`** model object
#'
#' @export
change_irs_coverage.multiround = function(coverage, xds_obj){
  xds_obj$irs_obj$cover_obj$coverage = coverage
  xds_obj$irs_obj$cover_obj <-  make_F_cover_irs(xds_obj$irs_obj$cover_obj)
  return(xds_obj)
}

#' @title Set up dynamic forcing
#'
#' @description Set up a function that computes
#' irs coverage over time.
#'
#' @param options a list of options to override defaults
#' @param t_init the time when IRS started
#' @param coverage the coverage achieved
#' @param type the IRS type
#' @param zap the contact parameter
#'
#' @return a IRS coverage model object
#' @export
make_irs_multiround = function(options=list(),
                                t_init = 0,
                                coverage = 0,
                                type = "none",
                                zap = 1){
  with(options,{
    nRounds <- length(t_init)
    stopifnot(length(coverage) == nRounds)
    stopifnot(length(type) == nRounds)

    cover <- list()
    cover$nRounds = nRounds
    class(cover) <- "multiround"
    cover$t_init = t_init
    cover$coverage = coverage
    cover$type = type
    cover$zap = checkIt(zap, nRounds)

    cover = make_F_cover_irs(cover)

    return(cover)
})}


#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param cover a coverage model object
#' @return a **`ramp.xds`** model object
#' @export
make_F_cover_irs = function(cover){

  rounds <- list()
  if(cover$nRounds==0) rounds[[1]] = makepar_F_zero()
  if(cover$nRounds>0)
    for(i in 1:cover$nRounds)
      rounds[[i]] = with(cover, make_irs_round(type[i], t_init[i], coverage[i], zap[i]))

  rounds_par <- makepar_F_multiround(cover$nRounds, rounds)
  cover$rounds <- rounds
  cover$rounds_par <- rounds_par
  cover$F_cover <- make_function(rounds_par)

  return(cover)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param xds_obj the `ramp.xds` model object
#' @param type the name of the IRS type
#' @param t_init the time when IRS started
#' @param coverage the coverage achieved
#' @param zap the coverage achieved
#' @return a **`ramp.xds`** model object
#' @export
add_irs_round = function(xds_obj, type, t_init, coverage, zap=1) {
  options <- list()
  options$type = c(xds_obj$irs_obj$cover_obj$type, type)
  options$t_init = c(xds_obj$irs_obj$cover_obj$t_init, t_init)
  options$coverage = c(xds_obj$irs_obj$cover_obj$coverage, coverage)
  options$zap = c(xds_obj$irs_obj$cover_obj$zap, zap)
  xds_obj$irs$cover_obj = make_irs_multiround(options)
  return(xds_obj)
}

