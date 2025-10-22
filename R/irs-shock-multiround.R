


#' @title Setup IRS Shocks
#'
#' @description Set up a function that computes
#' irs multi_objage over time.
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param options a list of options
#'
#' @return an IRS multi_objage / contact model object
#'
#' @export
setup_irs_shock_multiround = function(xds_obj, options=list()){
  N = xds_obj$events_obj$irs$N


  shock = rep(0, N)
  shock = with(options, shock)
  stopifnot(length(shock)==N)
  xds_obj$events_obj$irs$shock = shock


  pw = rep(1, N)
  pw = with(options, pw)
  stopifnot(length(pw)==N)
  xds_obj$events_obj$irs$pw = pw

  irs <- xds_obj$events_obj$irs
  xds_obj$EIR_obj$F_shock = make_F_multishock_irs(xds_obj)
  return(xds_obj)
}

#' @title Change IRS Shocks
#'
#' @description
#' For `eir` models, set up a multi-round
#' sharkbite function to model effect sizes
#' of IRS on the EIR as a perturbation.
#'
#' @param xds_obj an **`xds`** model object
#' @param shock the scaling parameter
#'
#' @export
change_irs_shock_multiround = function(xds_obj, shock){
  stopifnot(with(xds_obj, exists("events_obj")))
  stopifnot(with(xds_obj$events_obj, exists("irs")))

  stopifnot(length(shock) == xds_obj$events_obj$irs$N)
  xds_obj$events_obj$irs$shock = shock

  xds_obj$EIR_obj$F_shock = make_F_multishock_irs(xds_obj)
  return(xds_obj)
}

#' @title Change IRS Shocks
#'
#' @description
#' For `eir` models, set up a multi-round
#' sharkbite function to model effect sizes
#' of IRS on the EIR as a perturbation.
#'
#' @param xds_obj an **`xds`** model object
#' @param pw the shape parameter
#'
#' @export
change_irs_shock_pw_multiround = function(xds_obj, pw){
  stopifnot(with(xds_obj, exists("events_obj")))
  stopifnot(with(xds_obj$events_obj, exists("irs")))

  stopifnot(length(pw) == xds_obj$events_obj$irs$N)
  xds_obj$events_obj$irs$pw = pw

  xds_obj$EIR_obj$F_shock = make_F_multishock_irs(xds_obj)
  return(xds_obj)
}

#' @title Set up dynamic forcing
#'
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#'
#' @param xds_obj an **`xds`** model object
#'
#' @return a function
#' @export
make_F_multishock_irs = function(xds_obj){
  with(xds_obj$events_obj$irs,{
    stopifnot(length(peak)==N)
    checkIt(pw, N)

    rounds <- list()
    if(N>0)
      for(i in 1:N)
        rounds[[i]] = make_irs_shock(irs_type[i], start_day[i],
                                     shock[i], event_length[i], pw[i])

    rounds_par <- makepar_F_multishock(N, rounds)
    return(make_function(rounds_par))
  })}

#' Plot IRS Coverage
#'
#' @param tt a set of time points
#' @param xds_obj a **`ramp.xds`** model object
#' @param clr plotting color
#' @param add add to an existing plot
#'
#' @returns a **`ramp.xds`** xds_obj object
#' @export
show_irs_shock = function(tt, xds_obj, clr="black", add=FALSE){
  if(add==FALSE)
    graphics::plot(tt, xds_obj$EIR_obj$F_shock(tt), type = "n", xlab="Time (Days)", ylab = "Coverage")
  graphics::lines(tt, xds_obj$EIR_obj$F_shock(tt), col=clr)
}

