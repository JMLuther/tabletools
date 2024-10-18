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
#' calculate_creatclearance_cg(age=70, sex="MAle", weight=70, creatinine=1.0) # 68.05556
#' calculate_creatclearance_cg(age=70, sex="ma", weight=70, creatinine=1.0) # 57.84722
#' calculate_creatclearance_cg(age=50, sex="Male", weight=70, creatinine=1.0) # 87.5
#' calculate_creatclearance_cg(age=50, sex="F", weight=70, creatinine=1.0) # 74.375
#' calculate_creatclearance_cg(age=50, sex="fe", weight=154, creatinine=1.0, weight_units="lbs") # 74.375

calculate_creatclearance_cg <- function(age, sex, weight, creatinine, weight_units="kg") {
  sex=handle_sex(sex)
  F.sex = switch(sex, "Female" = 0.85, "Male"=1)
  weight_kg = convert_weight_to_kg(weight, weight_units)
  eGFR = (140 - age) * weight_kg * F.sex / (72*creatinine)
    return(eGFR)
}
