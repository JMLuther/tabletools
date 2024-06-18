#' Calculate HOMA-IR from fasting glucose and insulin
#'
#' @param glucose fasting glucose (mg/dL)
#' @param insulin fasting insulin (uU/mL)
#' @param glucose_units if units are not in "mg/dl", can indicate here for unit conversion (options "mg/dl" or "mmol/l") 
#' @param insulin_units if units are not in "uU/ml", can indicate here for unit conversion (options "uU/ml" or "pmol/l")
#'
#' @return HOMA-IR estimate of insulin sensitivity, as a single numeric value
#' @export
#'
#' @examples
#' calculate_homair(glucose = c(93), insulin = c(12.8))
#' calculate_homair(93, 12.8)
#' calculate_homair(100, 5) # 1.23; ex from Gutch 2015
#' calculate_homair(93, NA) # error
#' calculate_homair(5.167, 76.8, glucose_units = "mmol/l", insulin_units = "pmol/l")

calculate_homair <- function(glucose, insulin,  glucose_units = "mg/dl", insulin_units = "uU/ml") {
  if (any(!is.numeric(glucose)) | any(!is.numeric(insulin))) {
    return(NA_real_)
    warning("non-numeric values in glucose, or insulin")} 

  # convert units to glucose mg/dl and insulin uU/ml
  glucose = convert_glucose_to_mM(glucose, glucose_units)
  insulin = convert_insulin_to_uU_ml(insulin, insulin_units)

  return(glucose * insulin / 22.5) # formula if glucose in mM
  # return(glucose * insulin / 405) # formula if glucose in mg/dl; 22.5*18=405
  }

