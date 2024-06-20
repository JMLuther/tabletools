#' Calculate estimated GFR by Cockcroft-Gault
#'
#' @description Estimates Creatinine Clearance based on age, sex, weight, and
#'   creatinine. Note that other estimating equations estimate GFR.
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



calculate_creatclearance_cg <- function(age, sex, weight, creatinine) {
  F = switch(sex, "Female" = 0.85, "Male"=1)
  eGFR = (140 - age) * weight * F / (72*creatinine)
    return(eGFR)
}
