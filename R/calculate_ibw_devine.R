#' Ideal Body Weight Calculation
#'
#' This version is only the Devine method, and does not use the `switch()`
#' function. This did not avoid the vectorization issue, however, and it does
#' not work on dataframes without converting to a vectorized version. Kept only for documentation, since it is no faster than the other version.
#'
#' @rdname calculate_ibw
#' 
# calculate_ibw_devine(70, gender = "F")
# calculate_ibw_devine(70, gender = "M")
# calculate_ibw_devine(70, gender = "F", weight_units = "lbs")
# calculate_ibw_devine(70, gender = "F", weight_units = "kg")
# calculate_ibw_devine(1.778, gender = "F", height_units = "m")
# calculate_ibw_devine(1.778, gender = "M", height_units = "m")
# calculate_ibw_devine_v(ht_in, gender_v)


calculate_ibw_devine <- function(height, gender=NA,  
                                 weight_units = "kg",
                                 height_units = "in", 
                                 method = "Devine") {
  
  if (any(is.na(height) | any(is.na(gender)))) {
    rlang::warn("Check for missing values in height, gender")
    return(NA_real_)}
  
  if (any(!is.numeric(height))) {
    rlang::warn("non-numeric values in height")
    return(NA_real_)}
  
  # convert height and weight units to cm, kg
  ht_m = convert_length_to_m(length = height, length_units = height_units)
  ht_in = convert_length_to_in(length = height, length_units = height_units)
  
  # choose proper method 
  ibw_lbs = ifelse(gender %in% c("Female", "female", "F", "f"), 2.2*(45.5 + 2.3*(ht_in - 60)),
                   ifelse(gender %in% c("Male", "male", "M", "m"), 2.2*(50 + 2.3*(ht_in - 60)),
                          stop("invalid Gender: use Male/Female or M/F")))
  # Return using desired units, convert from lbs as needed
  ibw_final = ifelse(weight_units %in% c("lbs","pounds", "LBS", "lb"), ibw_lbs,
                     ifelse(weight_units %in% c("kg", "kilograms", "KG"), convert_weight_to_kg(ibw_lbs, weight_units = "lbs"),
                            stop("invalide weight_units: use kg or lbs")))
  return(ibw_final)
}

