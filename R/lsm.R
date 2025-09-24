#' @title Set Up the Null LSM Model Object
#'
#' @description
#' Sets up the null LSM model object
#'
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @return a **`ramp.xds`** model object
#'
#' @export
setup_lsm_object = function(xds_obj){
  xds_obj$lsm_obj = make_none_object()
  return(xds_obj)
}
