#' Calculate Urine Albumin/Creatinine ratio from Dipstick Urinalysis Protein
#'
#' Estimated urine albumin creatinine ratio by converting urine
#' protein/creatinine measurement, or by estimation from dipstick urinalysis
#' (negative, trace, +, ++, +++). methods described in
#' \href{https://pubmed.ncbi.nlm.nih.gov/32658569/}{Sumida et al. Annals Int
#' Med. 202}.
#'
#' @param ua_protein Dipstick UA protein result (negative, trace, +, ++, +++)
#' @param gender Gender (Female/Male)
#' @param diabetic Diabetic (T/F)
#' @param hypertensive Hypertensive (T/F) using prior cutoff of >140/90 or on
#'   meds
#' @param PCR_units urine Protein/creatinine units ("mg/g" default)
#'
#' @return Dataframe with predicted ACR (pACR) and estimated error
#' @export
#'
#' @examples
#' # calculate_uacr_from_ua(ua_protein = "trace", gender = "female", diabetic = T, hypertensive = T)
#' calculate_uacr_from_ua(ua_protein = "+", gender = "female", diabetic = T, hypertensive = T)
#' calculate_uacr_from_ua(ua_protein = "++", gender = "female", diabetic = T, hypertensive = T)
#' calculate_uacr_from_ua(ua_protein = "+++", gender = "female", diabetic = T, hypertensive = T)
#' 
#' calculate_uacr_from_ua(ua_protein = "trace", gender = "male", diabetic = T, hypertensive = T)
#' calculate_uacr_from_ua(ua_protein = "+", gender = "male", diabetic = T, hypertensive = T)
#' calculate_uacr_from_ua(ua_protein = "++", gender = "male", diabetic = T, hypertensive = T)
#' calculate_uacr_from_ua(ua_protein = "+++", gender = "male", diabetic = T, hypertensive = T)

calculate_uacr_from_ua <- function(ua_protein, gender, 
                                   diabetic=NA,
                                   hypertensive=NA) {

  # validate gender
  gender = tolower(gender)
  gender_query =
    ifelse(gender %in% c("female", "women", "fe", "f"), "female",
           ifelse(gender %in% c("male", "men", "ma", "m"), "male", NA))
  female = gender_query =="female"

  # Estimate UACR from dipstick
  ua_pr_exp =
    ifelse(ua_protein %in% c("negative", "-"), 0,
           ifelse(ua_protein %in% c("trace"), 0.7270,
                  ifelse(ua_protein %in% c("+", "1+"), 1.6775,
                         ifelse(ua_protein %in% c("++", "2+"), 3.2622,
                                ifelse(ua_protein %in% c("+++", "3+", ">2+", ">++"), 4.5435, NA)
                         ))))
  
  pACR = exp(2.0373 + 
               ua_pr_exp +
               ifelse(female, 0.0822, 0) + 
               ifelse(diabetic, 0.27249, 0) +
               ifelse(hypertensive, 0.33627, 0))
  
  pErr = sqrt(exp(-0.4525 + 
                    0.5939*log(min(pACR/30, 1))-
                    0.1292*log(max(min(pACR/300, 1), 0.1))-
                    0.2610*log(max(pACR/300, 1))-
                    ifelse(female, 0.0772, 0) + 
                    ifelse(diabetic, 0.2093, 0) +
                    ifelse(hypertensive, 0.1624, 0)))
  res = data.frame("pACR"=pACR,
                   "pACR_err"=pErr)
  return(res)
}

