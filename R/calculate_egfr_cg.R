#' Calculate estimated GFR by Cockcroft-Gault
#'
#' @description Calculates estimated glomerular filtration rate (GFR) by several
#'   common methods. Converts weight to Kg and Height to cm if needed. Available
#'   methods for eGFR calculations include:
#'
#'   *  CKD-EPI(2021)
#'   *  CKD-EPI Cystatin-C
#'   *  MDRD
#'   *  Cockcroft-Gault  
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
#' calculate_egfr_cg(age=70, sex="Male", weight=70, creatinine=1.0) # 95.2
#' calculate_egfr_cg(age=70, sex="Female", weight=70, creatinine=1.0) # 95.2
#' calculate_egfr_cg(age=50, sex="Male", weight=70, creatinine=1.0) # 95.2
#' calculate_egfr_cg(age=50, sex="Female", weight=70, creatinine=1.0) # 95.2



calculate_egfr_cg <- function(age, sex, weight, creatinine) {
  F = switch(sex, "Female" = 0.85, "Male"=1)
  eGFR = (140 - age) * weight * F / (72*creatinine)
    return(eGFR)
}
