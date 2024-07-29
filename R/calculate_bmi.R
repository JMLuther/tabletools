#' Calculate Body Mass Index (BMI)
#'
#' Uses weight (kg) and height (m) to calculate BMI. Converts from lbs and inches if needed and units are properly supplied.
#'
#' @param height height in cm, m, or inches
#' @param weight weight in lbs or kg
#' @param height_units units in "cm", "m", or "inches"
#' @param weight_units units as "lbs" or "kg"
#'
#' @return numeric vector containing BMI results
#' @export
#'
#' @examples
#' calculate_bmi(height = 1.778, weight = 75)
#' 
#' calculate_bmi(height = 70, weight = 165,
#'               height_units = "inches", weight_units = "lbs")
#'               
#' #incorrect units
#' calculate_bmi(height = 70, weight = 165,
#'               height_units = "in", weight_units = "lbs")
#'               
#' calculate_bmi(height = c(70, 72, 65), 
#'               weight = c(165, 180, 155),
#'               height_units = "inches", weight_units = "lbs")


calculate_bmi <- function(height, weight, height_units = "m", weight_units = "kg"){
  if(!height_units %in% c("cm", "m", "inches", "in", "ins")) rlang::abort("please specify height as cm, m, or in")
  if(!weight_units %in% c("kg", "g", "lbs")) rlang::abort("please specify weight units as kg, g, or lbs")
  
  weight_kg = convert_weight_to_kg(weight, weight_units)
  height_m =  convert_height_to_m(height, height_units)
  
  weight_kg/height_m^2
}
