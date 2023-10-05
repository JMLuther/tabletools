#' Quick Index of Insulin Sensitivity (QUICKI)
#'
#' @param glucose fasting glucose (mg/dL)
#' @param insulin fasting insulin (uU/mL)
#' @param glucose_units if units are not in "mg/dl", can indicate here for unit conversion (options "mg/dl" or "mmol/l") 
#' @param insulin_units if units are not in "uU/ml", can indicate here for unit conversion (options "uU/ml" or "pmol/l")
#'
#' @return QUICKI estimate of insulin sensitivity, as a single numeric value
#' @export
#'
#' @examples
#' calculate_quickiglucose = c(93), insulin = c(12.8))
#' calculate_quicki93, 12.8)
#' calculate_quicki93, NA) # error
#' calculate_quicki5.167, 76.8, glucose_units = "mmol/l", insulin_units = "pmol/l")

calculate_quicki <- function(glucose, insulin,  glucose_units = "mg/dl", insulin_units = "uU/ml") {
  if (any(!is.numeric(glucose)) | any(!is.numeric(insulin))) {
    return(NA_real_)
    warning("non-numeric values in glucose, or insulin")} 

  # convert units to glucose mg/dl and insulin uU/ml
  glucose = convert_glucose_to_mgdl(glucose, glucose_units)
  insulin = convert_insulin_to_uU_ml(insulin, insulin_units)

  return(1/((log(glucose, 10) + log(insulin, 10))))
  }

