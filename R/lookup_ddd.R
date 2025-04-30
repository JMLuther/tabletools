#' Lookup Defined Daily Dose (DDD) for hypertension medications
#'
#' Lookup function for DDD. Provides values only for hypertension related
#' medications. Medication name includes only the name of the
#' active ingredient, not the brand name (e.g. "clonidine" not "clonidine
#' hydrochloride"). The function will internally convert to lowercase for
#' matching purposes to the DDD list.
#'
#' @param medication Medication name (internally converted to lowercase)
#' @returns Reference DDD value for the medication provided
#' @export
#'
#' @examples
#' # example data
#' htn_med_dose
#' htn_med_dose |>
#'   mutate(
#'     ddd_value = lookup_ddd(medication), # example only - not required
#'     ddd_calc = calculate_ddd(medication, dose)) 

lookup_ddd <- function(medication){
  ddd = tabletools:::ddd
  ddd_values <- ddd$ddd[match(tolower(medication), ddd$name)]
  return(ddd_values)
}

