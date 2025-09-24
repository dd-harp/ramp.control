# Methods to set up variables describing exogenous forcing by care seeking

#' @title Set the values of exogenous variables describing care seeking
#' @description This method dispatches on the type of `xds_obj$CARE_SEEKING`.
#' @param t current simulation time
#' @param y state variables
#' @param xds_obj a  **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
CareSeeking <- function(t, y, xds_obj) {
 UseMethod("CareSeeking", xds_obj$CARE_SEEKING)
}

#' @title Set the values of exogenous variables describing care seeking
#' @description Implements [CareSeeking] for the no_behavior model of care seeking (do nothing)
#' @inheritParams CareSeeking
#' @return a **`ramp.xds`** model object
#' @export
CareSeeking.no_behavior <- function(t, y, xds_obj) {
 return(xds_obj)
}

#' @title Make parameters for the no_behavior model for care seeking (do nothing)
#' @param xds_obj a  **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
setup_care_seeking_no_behavior <- function(xds_obj) {
 CARE_SEEKING <- list()
 class(CARE_SEEKING) <- 'no_behavior'
 xds_obj$CARE_SEEKING <- CARE_SEEKING
 return(xds_obj)
}

