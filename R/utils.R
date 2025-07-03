
#' Make an empty list of class "none"
#'
#' @returns an empty list of class "none"
#' @export
none_obj = function(){
 none_obj <- list()
 class(none_obj) <- "none"
 none_obj$name <- "none"
 return(none_obj)
}
