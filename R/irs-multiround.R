
#' @title Set up dynamic forcing
#'
#' @description Set up a function that computes
#' irs multi_objage over time.
#'
#' @param contact the contact parameters to use
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @return an IRS multi_objage / contact model object
#'
#' @export
make_irs_multiround = function(contact, xds_obj){
  with(xds_obj$events_obj, stopifnot(exists("irs")))
  irs <- xds_obj$events_obj$irs
  irs$contact <- contact
  make_F_multi_irs(irs)
}


#' @title Set up dynamic forcing
#'
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#'
#' @param multi_obj a multiround model object
#'
#' @return a function
#' @export
make_F_multi_irs = function(multi_obj){

  rounds <- list()
  if(multi_obj$N>0)
    for(i in 1:multi_obj$N)
      rounds[[i]] = with(multi_obj, make_irs_round(type[i], jdate[i], peak[i], contact[i]))

  rounds_par <- makepar_F_multiround(multi_obj$N, rounds)
  multi_obj$rounds <- rounds
  multi_obj$rounds_par <- rounds_par
  F_multi <- make_function(rounds_par)

  return(F_multi)
}
