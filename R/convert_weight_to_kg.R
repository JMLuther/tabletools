#' Convert weight to kg
#'
#' @param weight weight in lbs g, or kg
#' @param weight_units units as "lbs" "g", or "kg"
#'
#' @return vector of weight in kg
#' @export
#'
#' @examples
#' convert_weight_to_kg(165, "lbs")
#' convert_weight_to_kg(165, "pounds")
#' convert_weight_to_kg(c(165, NA), "lbs")
#' convert_weight_to_kg(70000, "g")

convert_weight_to_kg <- function(weight, weight_units){
  switch(weight_units,
         kg = weight, kilos = weight, kilograms = weight,
         g = weight/1000, grams = weight/1000,
         lbs = weight/2.2, pounds  = weight/2.2,
         stop("invalid units; use kg, g, or lbs"))
}
