#' Convert weight to lbs
#' @rdname convert_weight_to_kg

convert_weight_to_lbs <- function(weight, weight_units){
  switch(weight_units,
         kg = weight*2.2, kilos = weight*2.2, kilograms = weight*2.2,
         g = weight*2.2/1000, grams = weight*2.2/1000,
         lbs = weight, pounds  = weight,
         stop("invalid units; use kg, g, or lbs"))
}
