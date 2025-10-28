
#' @title Setup Bed Net Shocks
#'
#' @description Set up a function that computes
#' bednet multi_objage over time.
#'
#' @param xds_obj an **`xds`**  model object
#' @param options a list of options
#'
#' @return an **`xds`** model object
#'
#' @export
setup_bednet_shock_multiround = function(xds_obj, options=list()){
  N = xds_obj$events_obj$bednet$N

  shock = rep(0, N)
  shock = with(options, shock)
  stopifnot(length(shock)==N)
  xds_obj$events_obj$bednet$shock = shock

  pw = rep(1, N)
  pw = with(options, pw)
  stopifnot(length(pw)==N)
  xds_obj$events_obj$bednet$pw = pw

  xds_obj$EIR_obj$F_shock = make_F_multishock_bednet(xds_obj)
  return(xds_obj)
}

#' @title Change bednet Shocks
#'
#' @description
#' For `eir` models, set up a multi-round
#' sharkbite function to model effect sizes
#' of bednet on the EIR as a perturbation.
#'
#' @param xds_obj an **`xds`** model object
#' @param shock the scaling parameter
#'
#' @return an **`xds`** model object
#'
#' @export
change_bednet_shock_multiround = function(xds_obj, shock){
  stopifnot(with(xds_obj, exists("events_obj")))
  stopifnot(with(xds_obj$events_obj, exists("bednet")))

  stopifnot(length(shock) == xds_obj$events_obj$bednet$N)
  xds_obj$events_obj$bednet$shock = shock

  xds_obj$EIR_obj$F_shock = make_F_multishock_bednet(xds_obj)
  return(xds_obj)
}

#' @title Change bednet Shocks
#'
#' @description
#' For `eir` models, set up a multi-round
#' sharkbite function to model effect sizes
#' of bednet on the EIR as a perturbation.
#'
#' @param xds_obj an **`xds`** model object
#' @param d_50 the scaling parameter
#'
#' @return an **`xds`** model object
#'
#' @export
change_bednet_shock_d50_multiround = function(xds_obj, d_50){
  stopifnot(with(xds_obj, exists("events_obj")))
  stopifnot(with(xds_obj$events_obj, exists("bednet")))

  stopifnot(length(d_50) == xds_obj$events_obj$bednet$N)
  xds_obj$events_obj$bednet$d_50 = d_50

  xds_obj$EIR_obj$F_shock = make_F_multishock_bednet(xds_obj)

  return(xds_obj)
}

#' @title Change bednet Shocks
#'
#' @description
#' For `eir` models, set up a multi-round
#' sharkbite function to model effect sizes
#' of bednet on the EIR as a perturbation.
#'
#' @param xds_obj an **`xds`** model object
#' @param pw the shape parameter
#'
#' @return an **`xds`** model object
#'
#' @export
change_bednet_shock_pw_multiround = function(xds_obj, pw){
  stopifnot(with(xds_obj, exists("events_obj")))
  stopifnot(with(xds_obj$events_obj, exists("bednet")))

  stopifnot(length(pw) == xds_obj$events_obj$bednet$N)
  xds_obj$events_obj$bednet$pw = pw

  xds_obj$EIR_obj$F_shock = make_F_multishock_bednet(xds_obj)
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
#'
#' @export
make_F_multishock_bednet = function(xds_obj){
  with(xds_obj$events_obj$bednet,{
    stopifnot(length(shock)==N)
    checkIt(pw, N)

    rounds <- list()
    if(N>0)
      for(i in 1:N)
        rounds[[i]] = make_bednet_shock(d_50[i], d_shape[i], start_day[i],
                                     shock[i], event_length[i], pw[i])

    rounds_par <- makepar_F_multishock(N, rounds)
    return(make_function(rounds_par))
  })}

#' Plot bednet Coverage
#'
#' @param tt a set of time points
#' @param xds_obj a **`ramp.xds`** model object
#' @param clr plotting color
#' @param add add to an existing plot
#'
#' @returns a **`ramp.xds`** xds_obj object
#' @export
show_bednet_shock = function(tt, xds_obj, clr="black", add=FALSE){
  if(add==FALSE)
    graphics::plot(tt, xds_obj$EIR_obj$F_shock(tt), type = "n", xlab="Time (Days)", ylab = "Coverage")
  graphics::lines(tt, xds_obj$EIR_obj$F_shock(tt), col=clr)
}

