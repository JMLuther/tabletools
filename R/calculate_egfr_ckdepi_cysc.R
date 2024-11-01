#' Calculate estimated GFR by 2021 CKD-EPI creatine/Cystatin C equation
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
#' @param creatinine creatinine (mg/dL) 
#' @param cystatin cystatin C (mg/L)
#' 
#'
#' @returns a numeric vector with eGFR (ml/min/1.73m^2)
#' @export calculate_egfr_ckdepi_cysc
#' @rdname calculate_egfr_ckdepi_cysc
#'
#' @examples
#' calculate_egfr_ckdepi_cysc(age=50, sex = "Female", creatinine=1.0, cystatin=1.5) # 54
#' calculate_egfr_ckdepi_cysc(age=50, sex = "Male", creatinine=1.0, cystatin=1.5) # 54
#' calculate_egfr_ckdepi_cysc(age=50, sex = "Female", creatinine=0.6, cystatin=0.7) # 115
#' calculate_egfr_ckdepi_cysc(age=50, sex = "Male", creatinine=0.6, cystatin=0.7) # 123
#' calculate_egfr_ckdepi_cysc(age=50, sex = "Female", creatinine=0.6, cystatin=0.8) # 111
#' calculate_egfr_ckdepi_cysc(age=50, sex = "Male", creatinine=0.6, cystatin=0.8) # 118
#' 
#' # original has been vectorized to handle multiple results
#' data.frame(age=c(5,10,15,35, 50),
#'            sex=rep("Male",5),
#'            creatinine=abs(rnorm(5, 1.2)),
#'            CysC=rep(1.2,5)) |>
#'   dplyr::mutate(egfr= calculate_egfr_ckdepi_cysc(age, sex, creatinine, CysC))


calculate_egfr_ckdepi_cysc_nonv <- function(age, sex, creatinine, cystatin) {
  sex = handle_sex(sex)
  args = data.frame(stringsAsFactors = FALSE,
                    sex = c("Female","Female","Female",
                            "Female","Male","Male","Male","Male"),
                    creat_bin = c("<=0.7",">0.7","<=0.7",
                                  ">0.7","<=0.9",">0.9","<=0.9",">0.9"),
                    cys_bin = c("<=0.8","<=0.8",">0.8",
                                ">0.8","<=0.8","<=0.8",">0.8",">0.8"),
                    A = c(0.7, 0.7, 0.7, 0.7, 0.9, 0.9, 0.9, 0.9),
                    B = c(-0.219,-0.544,-0.219,-0.544,
                          -0.144,-0.544,-0.144,-0.544),
                    C = c(0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8),
                    D = c(-0.323,-0.323,-0.778,-0.778,
                          -0.323,-0.323,-0.778,-0.778))
  creat.f = switch(sex,
                   "Female" = as.character(cut(creatinine, breaks=c(0, 0.7, Inf), labels=c("<=0.7", ">0.7"))),
                   "Male" = as.character(cut(creatinine, breaks=c(0, 0.9, Inf), labels=c("<=0.9", ">0.9"))))
  cystc.f = as.character(cut(cystatin, breaks=c(0, 0.8, Inf), labels=c("<=0.8", ">0.8")))
  A = switch(sex, "Female" = 0.7, "Male"=0.9) # same for CKD-EPI and CKD-EPI Cystatin-C
  B = args$B[args$sex=={{sex}} & args$creat_bin==creat.f & args$cys_bin==cystc.f]
  C = 0.8
  D = args$D[args$sex=={{sex}} & args$creat_bin==creat.f & args$cys_bin==cystc.f]
  F.sex = switch(sex, "Female" = 0.963, "Male"=1)
  
  eGFR = 135 * (creatinine/A)^B * (cystatin/C)^D * 0.9961^age * F.sex
  return(eGFR)
}
calculate_egfr_ckdepi_cysc <- Vectorize(calculate_egfr_ckdepi_cysc_nonv)

