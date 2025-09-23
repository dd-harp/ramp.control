#' @title Set Up the Null Sugar Baits Model Object
#'
#' @description
#' Sets up the null sugar baits model object
#'
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @return a **`ramp.xds`** model object
#'
#' @export
setup_atsb_object = function(xds_obj){
  xds_obj$atsb_obj = make_none_object()
  return(xds_obj)
}
