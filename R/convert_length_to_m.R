#' Convert length units
#'
#' @param length length in cm, m, or inches
#' @param length_units input units in "cm", "m", or "inches"
#'
#' @return vector of length in desired units specified by function name
#' @export
#'
#' @examples
#' convert_length_to_mm(1.03, "cm")
#' convert_length_to_mm(1, "in")
#' convert_length_to_mm(1, "m")
#' convert_length_to_mm(c(1.03, 1, 1), "cm")
#' 
#' convert_length_to_m(70, "inches")
#' convert_length_to_m(c(70, 75, 65), "inches")

convert_length_to_m <- function(length, length_units){
  switch(length_units,
         m = length, meters = length,
         cm = length/100, centimeters = length/100,
         inches = length*0.0254, "in" = length*0.0254, ins = length*0.0254,
         stop("invalid units; use m, cm, or inches"))
}

