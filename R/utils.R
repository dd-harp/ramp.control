#' @title Make "none" object
#'
#' @description
#' Make a "none" object
#'
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @return a **`ramp.xds`** model object
#'
#' @export
make_none_object = function(xds_obj){
  none_list <- list()
  none_list$name <- "none"
  class(none_list) = "none"
  return(none_list)
}
