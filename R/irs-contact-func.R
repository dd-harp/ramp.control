
#' @title Set Up a IRS Contact Function
#'
#' @description
#' This sets up a function to set the value of bed net
#' contact
#'
#' @inheritParams setup_irs_contact
#' @export
setup_irs_contact.func = function(name="func", xds_obj, options=list()){
  class(xds_obj$irs_obj) = "dynamic"
  xds_obj$irs_obj$contact_obj <- make_irs_contact_func(options)
  return(xds_obj)
}


#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#'
#' @param options a list of options to override defaults
#' @param mean the mean irs_contact
#' @param F_season the seasonal signal in irs contact
#' @param season_par parameters to configure F_season
#' @param F_trend a temporal trend in irs contact
#' @param trend_par parameters to configure F_trend
#'
#' @return a **`ramp.xds`** model object
#' @export
make_irs_contact_func = function(options=list(),
                                  mean=1,
                                  F_season=F_flat,
                                  season_par=list(),
                                  F_trend=F_flat,
                                  trend_par=list()){
  with(options,{
    contact <- list()
    class(contact) <- 'func'
    contact$name <- 'func'
    contact$mean <- mean
    contact$F_season <- F_season
    if(length(season_par)>1){
      contact$season_par <- season_par
      contact$F_season <- make_function(season_par)
    }
    contact$F_trend <- F_trend
    if(length(trend_par)>1){
      contact$trend_par <- trend_par
      contact$F_trend <- make_function(trend_par)
    }
    return(contact)
  })}


#' @title Set no irs_contact
#' @description The null model for irs_contact
#' @inheritParams IRS_Contact
#' @return a **`ramp.xds`** model object
#' @export
IRS_Contact.func <- function(t, y, xds_obj) {
  with(xds_obj$irs_obj$contact_obj,{
    xds_obj$irs_obj$contact = pmin(pmax(0, mean*F_season(t)*F_trend(t)),1)
    return(xds_obj)
  })}
