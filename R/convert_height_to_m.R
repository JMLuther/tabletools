#' Convert height to m
#'
#' @param height height in cm, m, or inches
#' @param height_units units in "cm", "m", or "inches"
#'
#' @return vector of height in meters (m)
#' @export
#'
#' @examples
#' convert_height_to_m(70, "inches")
#' convert_height_to_m(c(70, 75, 65), "inches")

convert_height_to_m <- function(height, height_units){
  switch(height_units,
         m = height, meters = height,
         cm = height/100,
         inches = height*0.0254, "in" = height*0.0254, ins = height*0.0254,
         stop("invalid units; use m, cm, or inches"))
}
