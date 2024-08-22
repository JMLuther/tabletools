#' Calculate Mean Arterial Pressure 
#' 
#' Returns the mean arterial pressure (MAP) from standard systolic (SBP) and diastolic (DBP) blood pressure measurements
#'
#' @param sbp Systolic blood pressure, mmHg
#' @param dbp Diastolic blood pressure, mmHg
#'
#' @return map as a single value, or vector of results 
#' @export
#'
#' @examples
#' calculate_map(159, 46)
#' calculate_map("159", 46)
#' calculate_map("159", NA)

calculate_map <- function(sbp, dbp){
  if (any(is.na(sbp) | any(is.na(dbp)))) {
    rlang::warn("Check for missing values in sbp, dbp")
    return(NA_real_)}
  
  if (any(!is.numeric(sbp)) | any(!is.numeric(dbp))) {
    rlang::warn("non-numeric values in sbp or dbp")
    return(NA_real_)}
  
  map = sbp/3 + 2*dbp/3
  return(map)
} 


