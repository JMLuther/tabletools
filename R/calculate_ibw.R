#' Ideal Body Weight Calculation
#'
#' @description Calculates Ideal Body Weight (IBW) by several common methods.
#'   Converts weight to Kg and Height to cm if needed. Available methods for IBW
#'   calculations are described in
#'   \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4841935}{Peterson et
#'   al.}:
#'
#' @param height Body Height (m)
#' @param height_units If `height` is in Units other than m, specifiy here to
#'   properly convert for calucations ("m", "cm", or "in")
#' @param weight_units Desired units for result
#' @param method string for IBW formula to use for calculation ("Hamwi",
#'   "Devine", "Robinson", "Broca", "Miller", "Hammond")
#'
#' @returns a numeric vector with Ideal Body Weight (defaults to lbs)
#' @export
#' @md
#'
#' @examples
#' ibw_methods <- c("Hamwi", "Devine", "Robinson", "Broca", "Miller", "Hammond")
#' names(ibw_methods) <- ibw_methods
#' sapply(ibw_methods, \(x)calculate_ibw(height = 70, gender="M", method = x))
#' sapply(ibw_methods, \(x)calculate_ibw(height = 70, gender="F", method = x))
#' # if gender missing returns NA
#' sapply(ibw_methods, \(x)calculate_ibw(height = 70,  method = x))
#' 
#' # Change Units for desired IBW result
#' # if not specified defaults to LBS
#' sapply(ibw_methods, \(x)calculate_ibw(height = 70, gender="F", method = x))
#' sapply(ibw_methods, \(x)calculate_ibw(height = 70, gender="F", method = x, weight_units="kg"))
#' sapply(ibw_methods, \(x)calculate_ibw(height = 70, gender="F", method = x, weight_units="kilograms"))
#' sapply(ibw_methods, \(x)calculate_ibw(height = 70, gender="F", method = x, weight_units="g"))
#' sapply(ibw_methods, \(x)calculate_ibw(height = 70, gender="F", method = x, weight_units="lbs"))

calculate_ibw <- function(height, gender=NA,  
                          weight_units = "lbs",
                          height_units = "in", 
                          method = "Hamwi") {

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
         Hamwi= 100 + 5*(ht_in - 60),
         Devine= 2.2*(45.5 + 2.3*(ht_in - 60)), 
         Robinson = 2.2*(49 + 1.7*(ht_in - 60)), 
         Broca = 2.2*(ht_m*100 - 100),
         Miller = 2.2*(53.1 + 1.36*(ht_in - 60)),
         Hammond = 2.2*(45 + 0.9*(ht_m*100 - 150)),
         stop("invalid method; please use 'method = ' one of the following: 
              Hamwi, Devine, Robinson, Broca, Miller, Hammond" ))
  } else if (gender %in% c("Male", "MALE", "M", "male", "m")) {
    ibw_lbs = switch(method,
                     Hamwi= 106 + 6*(ht_in - 60),
                     Devine= 2.2*(50 + 2.3*(ht_in - 60)), 
                     Robinson = 2.2*(52 + 1.9*(ht_in - 60)), 
                     Broca = 2.2*(ht_m*100 - 100),
                     Miller = 2.2*(56.2 + 1.41*(ht_in - 60)),
                     Hammond = 2.2*(48 + 1.1*(ht_m*100 - 150)),
                     stop("invalid method; please use 'method = ' one of the following: 
              Hamwi, Devine, Robinson, Broca, Miller, Hammond" ))
  }
  # if (weight_units %in% c("lbs", "pounds", "lb")) {
  #   return(ibw_lbs) } else if (weight_units %in% c("kg", "kilograms", "kgs")) {
  #     return(ibw_lbs/2.2) } 
  return(switch(weight_units,
         lbs = ibw_lbs, lb=ibw_lbs, LBS=ibw_lbs,
         kg=ibw_lbs/2.2, kilograms=ibw_lbs/2.2, KG=ibw_lbs/2.2,
         grams=ibw_lbs/2.2*1000, g=ibw_lbs/2.2*1000, G=ibw_lbs/2.2*1000,
         stop("invalid weight_units; suggest lbs, kg, or g")))
  
  }
