#' Calculate estimated GFR by MDRD
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
#' @param race 
#' @param creatinine 
#'
#' @returns a numeric vector with eGFR (ml/min/1.73m^2)
#' @export
#'
#' @examples
#' 
#' calculate_egfr_ckdepi(age=50, sex="Male", creatinine=0.6) # 118
#' calculate_egfr_mdrd(age=50, sex="Male", creatinine=0.6) # 142.6
#' 
#' calculate_egfr_ckdepi(age=50, sex="Male", creatinine=1) # 92
#' calculate_egfr_mdrd(age=50, sex="Male", creatinine=1)   # 79
#' 
#' calculate_egfr_ckdepi(age=50, sex="Male", creatinine=1.5) # 56
#' calculate_egfr_mdrd(age=50, sex="Male", creatinine=1.5) # 49
#' 
#' calculate_egfr_ckdepi(age=50, sex="Male", creatinine=2.0) # 40
#' calculate_egfr_mdrd(age=50, sex="Male", creatinine=2.0) # 35.5



calculate_egfr_mdrd <- function(age, sex="Male", race="White", weight, creatinine) {
  F = switch(sex, "Female" = 0.742, "Male"=1)
  R = switch(race, "Black" = 1.212, "White"=1)
  eGFR = 175 * creatinine ^(-1.154) * age^(-0.203) * R * F
    return(eGFR)
}


