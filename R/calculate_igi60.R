#' @rdname calculate_igi30
#' @export

calculate_igi60 <- function(time, glucose, insulin, time_units = "min", 
                                    glucose_units = "mg/dl", insulin_units = "uU/ml") {
  
  if (any(is.na(time)) | any(is.na(glucose)) | any(is.na(insulin))) {
    rlang::warn("Check for missing values in time, glucose, and insulin")
    return(NA_real_)}
  
  if (any(!is.numeric(time)) | any(!is.numeric(glucose)) | any(!is.numeric(insulin))) {
    rlang::warn("non-numeric values in time, glucose, or insulin")
    return(NA_real_)}
  
  # if (!all(order(time) == seq_along(time))) {
  #   rlang::warn("Time values must be ordered from 0 to end") # check proper time order
  #   return(NA_real_)}
  
  # convert units; 
  time = convert_time_to_min(time, time_units)
  glucose = convert_glucose_to_mM(glucose, glucose_units)
  insulin = convert_insulin_to_uU_ml(insulin, insulin_units)
  
  ind0 = which(time==0)
  g0 = glucose[ind0]
  ins0 = insulin[ind0]

  ind60 = which(time==60)
  g60 = glucose[ind60]
  ins60 = insulin[ind60]
  
    return((ins60 - ins0)/(g60-g0))
}
