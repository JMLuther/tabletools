#' Calculate HOMA-IR from fasting glucose and insulin
#'
#' Uses the simple formulas \eqn{HOMA1-IR = Glucose(mM) \cdot Insulin(uU/mL) /22.5} and \eqn{HOMA1-Beta = (20 Insulin(uU/mL))/(Glucose(mM) - 3.5)} as described in \href{https://pubmed.ncbi.nlm.nih.gov/9839117/}{Levy et al.}
#'
#' @param glucose fasting glucose (mg/dL)
#' @param insulin fasting insulin (uU/mL)
#' @param glucose_units if units are not in "mg/dl", can indicate here for unit conversion (options "mg/dl" or "mmol/l")
#' @param insulin_units if units are not in "uU/ml", can indicate here for unit conversion (options "uU/ml" or "pmol/l")
#'
#' @return HOMA-IR estimate of insulin sensitivity, as a single numeric value
#' @export calculate_homa1_ir
#' @export calculate_homa1_beta
#' @export calculate_homa1_is
#'
#' @examples
#' calculate_homa1_ir(glucose = c(93), insulin = c(12.8))
#' calculate_homa1_ir(93, 12.8)
#' calculate_homa1_ir(100, 5) # 1.23; ex from Gutch 2015
#' calculate_homa1_ir(93, NA) # error
#' calculate_homa1_ir(5.167, 76.8, glucose_units = "mmol/l", insulin_units = "pmol/l")

calculate_homa1_ir <- function(
  glucose,
  insulin,
  glucose_units = "mg/dl",
  insulin_units = "uU/ml"
) {
  if (any(!is.numeric(glucose)) | any(!is.numeric(insulin))) {
    return(NA_real_)
    warning("non-numeric values in glucose, or insulin")
  }

  # convert units to glucose mg/dl and insulin uU/ml
  glucose = convert_glucose_to_mM(glucose, glucose_units)
  insulin = convert_insulin_to_uU_ml(insulin, insulin_units)

  return(glucose * insulin / 22.5) # formula if glucose in mM
  # return(glucose * insulin / 405) # formula if glucose in mg/dl; 22.5*18=405
}

calculate_homa1_is <- function(
  glucose,
  insulin,
  glucose_units = "mg/dl",
  insulin_units = "uU/ml"
) {
  if (any(!is.numeric(glucose)) | any(!is.numeric(insulin))) {
    return(NA_real_)
    warning("non-numeric values in glucose, or insulin")
  }

  # convert units to glucose mg/dl and insulin uU/ml
  glucose = convert_glucose_to_mM(glucose, glucose_units)
  insulin = convert_insulin_to_uU_ml(insulin, insulin_units)

  return(22.5 / (glucose * insulin)) # formula if glucose in mM
  # return(405/(glucose * insulin)) # formula if glucose in mg/dL
}

calculate_homa1_beta <- function(
  glucose,
  insulin,
  glucose_units = "mg/dl",
  insulin_units = "uU/ml"
) {
  if (any(!is.numeric(glucose)) | any(!is.numeric(insulin))) {
    return(NA_real_)
    warning("non-numeric values in glucose, or insulin")
  }

  # convert units to glucose mg/dl and insulin uU/ml
  glucose = convert_glucose_to_mM(glucose, glucose_units)
  insulin = convert_insulin_to_uU_ml(insulin, insulin_units)

  return(20 * insulin / (glucose - 3.5)) # formula if glucose in mM
  # return(360*insulin/(glucose - 63)) # formula if glucose in mM
}
