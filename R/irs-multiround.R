
#' @title Set up dynamic forcing
#'
#' @description Set up a function that computes
#' irs coverage over time.
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param use_contact TRUE uses contact parameters from the events object
#'
#' @return an IRS coverage model object
#'
#' @export
make_irs_multiround = function(xds_obj, use_contact=TRUE){
  with(xds_obj$events_obj, stopifnot(exists("irs")))
  with(xds_obj$events_obj$irs,{
    stopifnot(N>0)
    if(!exists("include")) include = rep(TRUE, N)
    nRounds <- sum(include)
    cover <- list()
    class(cover) <- "multiround"
    cover$nRounds = nRounds
    cover$start   = jdate[include]
    cover$peak    = peak[include]
    cover$type    = type[include]

    if(use_contact==TRUE){
      cover$contact = contact[include]
      cover$F_contact <- make_F_cover_irs(cover)
    } else {
      cover$contact = rep(1, nRounds)
      cover$F_cover <- make_F_cover_irs(cover)
    }
    return(cover)
})}


#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param cover a coverage model object
#' @return a function
#' @export
make_F_cover_irs = function(cover){

  rounds <- list()
  if(cover$nRounds==0) rounds[[1]] = makepar_F_zero()
  if(cover$nRounds>0)
    for(i in 1:cover$nRounds)
      rounds[[i]] = with(cover, make_irs_round(type[i], start[i], peak[i], zap[i]))

  rounds_par <- makepar_F_multiround(cover$nRounds, rounds)
  cover$rounds <- rounds
  cover$rounds_par <- rounds_par
  F_cover <- make_function(rounds_par)

  return(F_cover)
}
