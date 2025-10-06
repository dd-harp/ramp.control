#' Show Events
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param mn the bottom of the segment
#' @param mx the top of the segment
#' @param bclr a color for the bednet line segments
#' @param iclr a color for the irs line segments
#' @param add if TRUE, add to an existing plot
#'
#' @importFrom graphics points text
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
show_events = function(xds_obj, mn=0, mx=1, bclr="#E4460AFF", iclr = "#4686FBFF", add=FALSE){
  show_bednet_events(xds_obj, mn, mx, bclr)
  show_irs_events(xds_obj, mn, mx, iclr, add=TRUE)
  xds_obj$data$tt -> tt
  xds_obj$data$yy -> yy
  points(tt, 0*yy, pch=10, cex=1.5, col ="#30123BFF")
  text(tt, 0*yy, 1:length(tt), pos=3, col = "#30123BFF")
}
