#' @title Set Up the Null Area Spray Model Object
#'
#' @description
#' Sets up the null area spray model object
#'
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @return a **`ramp.xds`** model object
#'
#' @export
setup_area_spray_object = function(xds_obj){
  xds_obj$area_spray_obj = make_none_object()
  return(xds_obj)
}
