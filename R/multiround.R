
#' @title Make Parameters for a Multiround Function
#'
#' @description Return an object to configure
#' multiple `sharkfin` functions representing
#' coverage levels over many mass distribution events
#'
#' @param nRounds the rate parameter
#' @param rounds the rounds
#'
#' @return a multiround function
#'
#' @export
makepar_F_multiround = function(nRounds, rounds){

  if(nRounds ==1) rounds_par = rounds[[1]]
  if(nRounds > 1) rounds_par = makepar_F_nproduct(rounds[[1]], rounds[[2]])
  if(nRounds > 2)
    for(i in 3:nRounds)
      rounds_par = makepar_F_nproduct(rounds_par, rounds[[i]])

  return(rounds_par)
}

#' @title Make Parameters for a multishock Function
#'
#' @description Return an object to configure
#' multiple `sharkfin` functions representing
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
