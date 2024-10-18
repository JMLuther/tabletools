#' Calculate estimated GFR, European Kidney Function Consortium (EKFC) 2021 formula
#'
#' Calculates the European Kidney Function Consortium (EKFC) 2021 formula estimated
#'   glomerular filtration rate. Methods described in
#'   \href{https://pubmed.ncbi.nlm.nih.gov/33166224/}{Pottel et al, Annals Int Med. 2021}.
#'   An online calculator is found \href{https://ekfccalculator.pages.dev/?value=50&value=1&value=#results}{here}.
#'
#'
#' @param age Age, in years
#' @param sex Sex
#' @param creatinine Serum Creatinine, mg/dL
#' @param creatinine_units Creatinine units, if different than mg/dL
#'
#' @returns a numeric vector with eGFR (ml/min/1.73m^2)
#' @export calculate_egfr_ekfc
#' @rdname calculate_egfr_ekfc
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
#' 
#' # EKFC 2021: 
#' calculate_egfr_ekfc(1.2, age=50, sex="Male")
#' calculate_egfr_ekfc(1.2, age=19, sex="Ma")
#' calculate_egfr_ekfc(1.2, age=15, sex="male")
#' 
#' calculate_egfr_ekfc(1.2, age=50, sex="fe")
#' calculate_egfr_ekfc(1.2, age=19, sex="Female")
#' calculate_egfr_ekfc(1.2, age=15, sex="FE")
#' calculate_egfr_ekfc(1.2, age=10, sex="FEMALE")
#' calculate_egfr_ekfc(1.2, age=5, sex="Female")
#' 
#' # original has been vectorized to handle multiple results
#' data.frame(age=c(5,10,15,35, 50), 
#' sex=rep("Male",5),
#' creatinine=rep(1.2,5)) |> 
#' dplyr::mutate(egfr=calculate_egfr_ekfc(creatinine, age, sex))


calculate_egfr_ekfc_nonv <- function(creatinine, age, sex, 
                           creatinine_units = "mg/dL") {
  
  # creatinine and Q must be same units, doesn't matter which
  creatinine = convert_creatinine_to_uM(creatinine, creatinine_units)
  Q = get_Q_uM(age, sex)
  107.3*ifelse(creatinine/Q <1, (creatinine/Q)^(-0.322), (creatinine/Q)^(-1.132))*ifelse(age<40,1, 0.990^(age-40))
}
calculate_egfr_ekfc <- Vectorize(calculate_egfr_ekfc_nonv)

# this function is not vectorized; 
# Q represents the median creatinine for age, sex returns umol/L but this does
# not matter, since internally the main function converts units of creatinine to
# umol/L for consistency
get_Q_uM <- function(age, sex){
  sex = handle_sex(sex)
  if (age <2){stop("estimation not possible for age <2")}
  if (age<=40 & age >=2){
    if (sex == "Male") {
      ln_Q = 3.200 + 0.259*age-0.543 * log(age) - 0.00763*age^2 + 0.0000790*age^3 
      Q = exp(ln_Q) # umol/l; div by 88.4 to convert to mg/dl
    } else if (sex == "Female") {
      ln_Q = 3.080 + 0.177*age - 0.223*log(age)-0.00596*age^2 + 0.0000686*age^3
      Q = exp(ln_Q) # umol/l; div by 88.4 to convert to mg/dl 
    }
  }
  if (age>40){
    if (sex == "Male") {
      Q = 80 # umol/l; div by 88.4 to convert to mg/dl
    } else if (sex == "Female") {
      Q = 62 # umol/l; div by 88.4 to convert to mg/dl
    }
  }
  return(Q) # Q represents the median creatinine for age/sex
}
