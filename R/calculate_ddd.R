#' Calculating Defined Daily Dose (DDD) for hypertension medications
#'
#' Calculation function for DDD. Provides values only for hypertension related
#' medications. The dose is the total administered per day, regardless of
#' frequency of administration. Medication name includes only the name of the
#' active ingredient, not the brand name (e.g. "clonidine" not "clonidine
#' hydrochloride"). The function will internally convert to lowercase for
#' matching purposes to the DDD list.
#'
#' @param medication Medication name (internally converted to lowercase)
#' @param dose Dose of medication (in mg, total administered per day)
#'
#' @returns calculated DDD value for the medication and dose provided
#' @export
#'
#' @examples
#' # example data
#' head(htn_med_dose)
#' lookup_ddd(htn_med_dose$medication)
#' calculate_ddd(htn_med_dose$medication, htn_med_dose$dose)

calculate_ddd <- function(medication, dose){
  if (length(medication) != length(dose)) {
    stop("medication and dose vectors must be of the same length")
  }
  ddd_values = lookup_ddd(medication) # convert lowercase for matching
  ddd_calc = dose/ddd_values
  return(ddd_calc)
}
