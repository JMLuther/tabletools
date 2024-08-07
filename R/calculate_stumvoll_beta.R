#' Stumvoll beta cell function Calculation (from standard 75g OGTT)
#'
#' This function calculates the Stumvoll phase 1 (ph1) and phase 2 (ph2) using glucose
#' and insulin sampled during the first 30 minutes of a standard 75g oral glucose tolerance test. Described in \href{https://pubmed.ncbi.nlm.nih.gov/10868854/}{Stumvoll et al.}
#'
#' Standard timepoints are 0, 30, 60, 90, and 120 min. Note: insulin unit
#' conversion may differ differ depending on assay. Insulin (pmol/l) = insulin
#' (uU/ml)*6
#'
#' `calculate_stumvoll_beta()` accepts 3 separate vectors for time, glucose,
#' insulin. 
#'
#' @param time  a column name (unquoted) indicating time values (in minutes)
#' @param glucose a column name (unquoted) storing glucose values (in mg/dL)
#' @param insulin a column name (unquoted) storing insulin values (in uU/mL)
#' @param time_units if units are not in "min", can indicate here for unit
#'   conversion (options "min" or "hr")
#' @param glucose_units if units are not in "mg/dl", can indicate here for unit
#'   conversion (options "mg/dl" or "mmol/l")
#' @param insulin_units if units are not in "uU/ml", can indicate here for unit
#'   conversion (options "uU/ml" or "pmol/l")
#' @return Stumvoll beta cell estimates (`stumvoll_ph1`, `stumvol_ph2`) values as a `data.frame`
#'
#' @export
#' @examples
#' # individual objects for each item
#' time=c(0, 30, 60, 90, 120)              # minutes
#' glucose=c(93, 129, 178, 164, 97)        # mg/dL
#' insulin=c(12.8, 30.7, 68.5, 74.1, 44.0) # uU/mL
#' calculate_stumvoll_beta(time, glucose, insulin)

calculate_stumvoll_beta <- function(time, glucose, insulin, 
                                   time_units = "min", 
                                    glucose_units = "mg/dl", insulin_units = "uU/ml") {
  
  if (any(!is.numeric(time)) | any(!is.numeric(insulin))) {
    rlang::warn("non-numeric values in time, glucose, or insulin")
    return(NA_real_)}

  # convert units; 
  time = convert_time_to_min(time, time_units)
  glucose = convert_glucose_to_mM(glucose, glucose_units)
  insulin = convert_insulin_to_pM(insulin, insulin_units)
  
  ind0 = which(time==0)
  g0 = glucose[ind0]
  ins0 = insulin[ind0]

  ind30 = which(time==30)
  g30 = glucose[ind30]
  ins30 = insulin[ind30]
  
  if (any(is.na(ins30)) | any(is.na(g0)) ) {
    rlang::warn("Check for missing values in BMI, or Insulin t=120")
    return(NA_real_)}
  
  stumvoll_1 =  1283 + 1.829*ins30 - 138.7*g30 + 3.772*ins0
  stumvoll_2 =  287 + 0.4164*ins30 - 26.07*g30 + 0.9226*ins0
  
    return(list("stumvoll_ph1" = stumvoll_1, "stumvoll_ph2" = stumvoll_2))
}

