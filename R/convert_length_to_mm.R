#' Convert length to mm
#'
#' @param length length in cm, m, or inches
#' @param length_units units in "cm", "m", or "inches"
#'
#' @return vector of length in millimeters (mm)
#' @export
#'
#' @examples
#' convert_length_to_mm(1.03, "cm")
#' convert_length_to_mm(1, "in")
#' convert_length_to_mm(1, "m")
#' convert_length_to_mm(c(1.03, 1, 1), "cm")

convert_length_to_mm <- function(length, length_units){
  switch(length_units,
         mm = length, millimeters = length,
         m = length*1000, meters = length*1000,
         cm = length*10,
         inches = length*2.54*10, "in" = length*2.54*10, ins = length*2.54*10,
         stop("invalid units; use mm, m, cm, or inches"))
}
