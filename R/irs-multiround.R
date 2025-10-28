
#' @title Set up dynamic forcing
#'
#' @description Set up a function that computes
#' irs multi-round
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param peak the scaling parameter
#' @param pw a shape parameter
#'
#' @return an IRS multi_objage / contact model object
#'
#' @export
make_irs_multiround = function(xds_obj, peak, pw=1){
  with(xds_obj$events_obj, stopifnot(exists("irs")))
  irs_events <- xds_obj$events_obj$irs
  return(make_F_multiround_irs(irs_events, peak, pw))
}

#' @title Set up dynamic forcing
#'
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#'
#' @param multi_obj a multiround model object
#' @param peak the scaling parameter
#' @param pw a shape parameter
#'
#' @return a function
#' @export
make_F_multiround_irs = function(multi_obj, peak, pw=1){
  with(multi_obj,{
    stopifnot(length(peak)==N)
    pw = checkIt(pw, N)

    rounds <- list()
    if(N>0)
      for(i in 1:N)
        rounds[[i]] = make_irs_round(type[i], start_day[i], peak[i], event_length[i], pw[i])

    rounds_par <- makepar_F_multiround(multi_obj$N, rounds)
    return(make_function(rounds_par))
})}


