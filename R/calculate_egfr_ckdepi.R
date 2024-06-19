#' Calculate estimated GFR by CKD-EPI 2021 equation
#'
#' @description Calculates estimated glomerular filtration rate (GFR) by several
#'   common methods. Converts weight to Kg and Height to cm if needed. Avaialbe
#'   methods for eGFR calculations include:
#'
#'   *  CKD-EPI(2021): 142*(Cr/A)^B * 0.9938^age * (1.012 if Female)
#'   *  CKD-EPI Cystatin-C: 135 x (Scr/A)B x (Scys/C)D x 0.9961age x (0.963 if female)
#'   *  MDRD:
#'   *  Cockcroft-Gault:  
#'
#' @param age Age, in years
#' @param sex Sex 
#' @param creatinine Serum creatinine in mg/dL
#'
#' @returns a numeric vector with eGFR (ml/min/1.73m^2)
#' @export
#'
#' @examples
#' calculate_egfr(age=70, sex="Male", creatinine=0.8, method="CKD-EPI") # 95.2
#' calculate_egfr(age=50, sex="Female", creatinine=1.0, method="CKD-EPI") # 68.6  


calculate_egfr_ckdepi <- function(age, sex, creatinine) {
  args = data.frame(sex = c("Female", "Female", "Male", "Male"),
                    creat_bin = c("<=0.7", ">0.7", "<=0.9", ">0.9"),
                    A = c(0.7, 0.7, 0.9, 0.9),
                    B = c(-0.241, -1.2, -0.302, -1.2))
  creat.f = switch(sex,
                   "Female" = as.character(cut(creatinine, breaks=c(0, 0.7, Inf), labels=c("<=0.7", ">0.7"))),
                   "Male" = as.character(cut(creatinine, breaks=c(0, 0.9, Inf), labels=c("<=0.9", ">0.9"))))

  A = switch(sex, "Female" = 0.7, "Male"=0.9) # same for CKD-EPI and CKD-EPI Cystatin-C
  B = args$B[args$sex=={{sex}} & args$creat_bin==creat.f]
  F = switch(sex, "Female" = 1.012, "Male"=1)
  
  eGFR = 142*(creatinine/A)^B * 0.9938^age * F
  return(eGFR)
}

