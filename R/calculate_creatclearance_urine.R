#' Calculate 24hour creatinine clearance by 24-hour urine results
#'
#' @description Calculates Creatinine Clearance by 24hr urine collection results. 
#' CrCL typically overestimates GFR by 10-20% due to tubular secretion of creatinine. Other assumptions also apply including:
#' * Creatinine/renal function is in steady state
#' * Creatinine is filtered and not secreted (not true- leads to 10-20% overestimate of GFR)
#' * Complete and well-timed urine collection 
#'
#' @param age Age, in years
#' @param sex Sex
#' @param weight Weight, in kg
#' @param creatinine Serum creatinine, in mg/dL
#'
#' @returns a numeric vector with eGFR (ml/min/1.73m^2)
#' @export
#'
#' @examples
#' calculate_creatclearance_urine(urine_volume = 1000, creatinine_serum = 1.0, creatinine_urine = 100) # 69.4
#' calculate_creatclearance_urine(urine_volume = 1000, creatinine_serum = 1.0, creatinine_urine = 100,
#'                                    bsa_adjust = T) # error- need height, weight
#' calculate_creatclearance_urine(urine_volume = 1000, creatinine_serum = 1.0, creatinine_urine = 100,
#'                                    bsa_adjust = T, weight = 70, height = 1.5) # 70.3

calculate_creatclearance_urine <- function(urine_volume, creatinine_urine, creatinine_serum, urine_collection_time=24,
                                               bsa_adjust=FALSE, height=NULL, weight=NULL) {
  CrCL = creatinine_urine * urine_volume/creatinine_serum/urine_collection_time/60  # ml/min
  if (bsa_adjust==TRUE & any(is.null(height) ||is.null(weight)) ) {
    stop("Please enter height(m) and weight(kg) for BSA adjustment")
  } else if (bsa_adjust==TRUE & !any(is.null(height) ||is.null(weight)) ) {
    adj  = 1.73/tabletools::calculate_bsa(weight, height)
    return(CrCL* adj) # ml/min/1.73m2
  } else {
    return(CrCL)
  }
}
