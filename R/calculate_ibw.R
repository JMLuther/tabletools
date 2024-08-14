#' Ideal Body Weight Calculation
#'
#' @description Calculates Ideal Body Weight (IBW) by Devine formula (same as
#'   Hamwi), with several other available methods. Converts units if needed.
#'   Available methods for IBW calculations are described in
#'   \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4841935}{Peterson et
#'   al.}.
#'
#'   The function is not vectorized, so to perform multiple calculations, first
#'   create a vectorized version of the function as in the examples below
#'
#' @param height Body Height (m)
#' @param height_units If `height` is in Units other than m, specifiy here to
#'   properly convert for calucations ("m", "cm", or "in")
#' @param weight_units Desired units for result
#' @param method string for IBW formula to use for calculation ("Hamwi",
#'   "Devine", "Robinson", "Broca", "Miller", "Hammond")
#'
#' @returns a numeric vector with Ideal Body Weight (defaults to lbs)
#' @export calculate_ibw
#' @md
#'
#' @examples
#' ibw_methods <- c("Hamwi", "Devine", "Robinson", "Broca", "Miller", "Hammond")
#' names(ibw_methods) <- ibw_methods
#' sapply(ibw_methods, \(x)calculate_ibw(height = 70, gender="M", method = x))
#' sapply(ibw_methods, \(x)calculate_ibw(height = 70, gender="F", method = x))
#' # if gender missing returns NA
#' sapply(ibw_methods, \(x)calculate_ibw(height = 70,  method = x))
#' # Change Units for desired IBW result
#' # if not specified defaults to LBS
#' sapply(ibw_methods, \(x)calculate_ibw(height = 70, gender="F", method = x))
#' sapply(ibw_methods, \(x)calculate_ibw(height = 70, gender="F", method = x, weight_units="kg"))
#' sapply(ibw_methods, \(x)calculate_ibw(height = 70, gender="F", method = x, weight_units="kilograms"))
#' sapply(ibw_methods, \(x)calculate_ibw(height = 70, gender="F", method = x, weight_units="g"))
#' sapply(ibw_methods, \(x)calculate_ibw(height = 70, gender="F", method = x, weight_units="lbs"))
#'
#' # use different input height, indicate using "height_units" argument
#' calculate_ibw(height = 1.778, gender="F", height_units = "m")
#' calculate_ibw(height = 177.8, gender="F", height_units = "cm")
#'
#'
#' dat <- data.frame(id =1:10,
#' gender = sample(c("M", "F", NA), 10, replace = T),
#' height = rnorm(10, 1.7, sd=0.2),
#' weight = rnorm(10, 100, sd=20))
#' library(dplyr)
#' dat |>
#'   mutate(ibw = calculate_ibw(height, gender, height_units = "m"))

calculate_ibw_nonvectorized <- function(height, gender=NA,  
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
  
  if (gender %in% c("Female", "FEMALE", "F", "female", "f", "fe")) {
    # choose proper method 
    ibw_lbs = switch(method,
                     Devine= 2.2*(45.5 + 2.3*(ht_in - 60)), 
                     Hamwi= 100 + 5*(ht_in - 60),
                     Robinson = 2.2*(49 + 1.7*(ht_in - 60)), 
                     Broca = 2.2*(ht_m*100 - 100),
                     Miller = 2.2*(53.1 + 1.36*(ht_in - 60)),
                     Hammond = 2.2*(45 + 0.9*(ht_m*100 - 150)),
                     stop("invalid method; please use 'method = ' one of the following: 
              Hamwi, Devine, Robinson, Broca, Miller, Hammond" ))
  } else if (gender %in% c("Male", "MALE", "M", "male", "m")) {
    ibw_lbs = switch(method,
                     Devine= 2.2*(50 + 2.3*(ht_in - 60)), 
                     Hamwi= 106 + 6*(ht_in - 60),
                     Robinson = 2.2*(52 + 1.9*(ht_in - 60)), 
                     Broca = 2.2*(ht_m*100 - 100),
                     Miller = 2.2*(56.2 + 1.41*(ht_in - 60)),
                     Hammond = 2.2*(48 + 1.1*(ht_m*100 - 150)),
                     stop("invalid method; please use 'method = ' one of the following: 
              Hamwi, Devine, Robinson, Broca, Miller, Hammond" ))
  } # Return using desired units, convert from lbs as needed
  return(switch(weight_units,
                lbs = ibw_lbs, lb=ibw_lbs, LBS=ibw_lbs,
                kg=ibw_lbs/2.2, kilograms=ibw_lbs/2.2, KG=ibw_lbs/2.2,
                grams=ibw_lbs/2.2*1000, g=ibw_lbs/2.2*1000, G=ibw_lbs/2.2*1000,
                stop("invalid weight_units; suggest lbs, kg, or g")))
  
}

# the function needs to be vectorized to work properly for multiple calculations
calculate_ibw <- Vectorize(calculate_ibw_nonvectorized)
