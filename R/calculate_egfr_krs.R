#' Calculate estimated GFR, Kidney transplant Recipient Specific (KRS)
#'
#' @description Calculates the Race-free kidney-transplant-specific estimated
#'   glomerular filtration rate. Methods described in
#'   \href{https://pubmed.ncbi.nlm.nih.gov/37257905/}{Raynaud et al. BMJ. 2023}.
#'   An online calculator is found at
#'   \href{https://transplant-prediction-system.shinyapps.io/eGFR_equation_KTX/}
#'
#'
#' @param age Age, in years
#' @param sex Sex
#' @param creatinine Serum Creatinine, mg/dL
#'
#' @returns a numeric vector with eGFR (ml/min/1.73m^2)
#' @export calculate_egfr_krs
#'
#' @examples
#' # CKD-EPI 2021 version (new race-free creatinine-based equation)
#' # https://www.kidney.org/professionals/kdoqi/gfr_calculator
#' calculate_egfr_ckdepi(age=50, sex="Male", creatinine=0.6) # 95.2
#' calculate_egfr_ckdepi(age=70, sex="Male", creatinine=0.8, version = "2021") # 95.2
#' calculate_egfr_ckdepi(age=70, sex="Female", creatinine=0.8, version = "2021") # 95.2
#'
#' # KRS 2023 version : transplant specific
#' calculate_egfr_krs(age=50, sex="Male", creatinine=1)
#' calculate_egfr_krs(age=50, sex="Female", creatinine=1)
#' calculate_egfr_krs(age=75, sex="Male", creatinine=1)
#' calculate_egfr_krs(age=75, sex="Female", creatinine=1)



calculate_egfr_krs <- function(age, sex, creatinine) {
    a = 4.4275492
    b = 0.8230475*log(creatinine) # mg/dL
    c = 0.0124264*creatinine^2 # mg/dL
    d = 0.0055068*age # years
    sex.f = 
      ifelse(sex %in% c("Female", "F", "female"), 0,
             ifelse(sex %in% c("Male", "M", "male"), 0.1806494, NA))
    eGFR = exp(4.4275492-b-c-d+sex.f)
  return(eGFR)
}
