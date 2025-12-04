
#' @title Make Parameters for a multishock Function
#'
#' @description Return an object to configure
#' multiple `sharkbite` functions representing
#' coverage levels over many mass distribution events
#'
#' @param nRounds the rate parameter
#' @param rounds the rounds
#'
#' @return a multishock function
#'
#' @export
makepar_F_multishock = function(nRounds, rounds){
  if(nRounds ==1) rounds_par = rounds[[1]]
  if(nRounds > 1) rounds_par = makepar_F_product(rounds[[1]], rounds[[2]])
  if(nRounds > 2)
    for(i in 3:nRounds)
      rounds_par = makepar_F_product(rounds_par, rounds[[i]])

  return(rounds_par)
}

#' @title Make `F_coverage` for IRS
#'
#' @description Set up the IRS rounds
#'
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @return set up the rounds
#'
#' @export
setup_F_multishock = function(xds_obj){
  rounds = list()
  if(with(xds_obj$events_obj, exists("irs"))){
    xds_obj <- setup_irs_rounds(xds_obj, xds_obj$events_obj$irs$shock, as_shock=TRUE)
    rounds = c(rounds, xds_obj$events_obj$irs$rounds)
  }
  if(with(xds_obj$events_obj, exists("bednet"))){
    xds_obj <- setup_bednet_rounds(xds_obj, xds_obj$events_obj$bednet$shock, as_shock=TRUE)
    rounds = c(rounds, xds_obj$events_obj$bednet$rounds)
  }
  N = length(rounds)
  pars  <- makepar_F_multishock(N, rounds)

  xds_obj$EIR_obj$shock_par <- pars
  xds_obj$EIR_obj$F_shock <- make_function(pars)

  return(xds_obj)
}

