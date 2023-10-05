#' Body Mass Index Calculation
#'
#' @description Calculation for body mass index (BMI, in kg/m^2), with unit conversion if needed.
#'
#' @param weight Body Weight (kg)
#' @param height Body Height (m)
#' @param weight_units If `weight` is in Units other than kg, specifiy here to  properly convert for calucations ("kg", "g", or "lbs")
#' @param height_units If `height` is in Units other than m, specifiy here to  properly convert for calucations ("m", "cm", or "in")
#'
#' @returns a numeric vector with Body Mass Index (BMI, kg/m^2)
#' @export
#'
#' @examples
#' bmi(weight = 70, height = 1.778)

#' # different units
#' bmi(weight = 154, weight_units = "lbs", 
#'     height = 70, height_units = "in", 
#'     method = "Mosteller")
#' bmi(weight = 70, height = 1.778)
#' 
#' @param weight 
#' @param height 
#' @param weight_units 
#' @param height_units 
bmi <- function(weight, height, weight_units = "kg", height_units = "m") {
  # convert weight units to kg
  if (weight_units == "kg") {
    Wt = weight
  } else if (weight_units == "g") {
    Wt = weight *1000
  } else if (weight_units == "lbs") {
    Wt = weight / 2.2}
  
  # convert height units to cm
  if (height_units == "m") {
    Ht = height 
  } else if (height_units == "cm") {
    Ht = height/100
  }  else if (height_units == "in") {
    Ht = height*2.54/100}
  
  # return the calculated result
  return(Wt/Ht^2)
}
