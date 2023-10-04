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
#' quicki(glucose = c(93), insulin = c(12.8))
#' quicki(93, 12.8)
#' quicki(93, NA) # error
#' quicki(5.167, 76.8, glucose_units = "mmol/l", insulin_units = "pmol/l")

quicki <- function(glucose, insulin,  glucose_units = "mg/dl", insulin_units = "uU/ml") {
  if (any(!is.numeric(glucose)) | any(!is.numeric(insulin))) {
    stop("non-numeric values in glucose, or insulin")} 
  # convert time glucose to mg/dl
  if (glucose_units == "mg/dl") {
    glucose = glucose
  } else if (glucose_units == "mmol/l") {
    glucose = glucose*18}
  
  # convert insulin units to uU/ml
  if (insulin_units == "uU/ml") {
    insulin = insulin
  } else if (insulin_units == "pmol/l") {
    insulin = insulin/6} # VUMC conversion units in DRTC lab
  return(1/((log(glucose, 10) + log(insulin, 10))))

  }

