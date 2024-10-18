#' Calculate Urine Albumin/Creatinine ratio from Protein/Creatinine ratio
#'
#' Estimated urine albumin creatinine ratio by converting urine
#' protein/creatinine measurement, or by estimation from dipstick urinalysis
#' (negative, trace, +, ++, +++). methods described in
#' \href{https://pubmed.ncbi.nlm.nih.gov/32658569/}{Sumida et al. Annals Int
#' Med. 202}.
#'
#' @param PCR urine Protein/creatinine ratio (mg/g creat.)
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
#' calculate_uacr_from_upcr(PCR = 1000, gender = "female", diabetic = T, hypertensive = T)
#' calculate_uacr_from_upcr(PCR = 500.0, gender = "female", diabetic = T, hypertensive = T)
#' calculate_uacr_from_upcr(PCR = 150, gender = "female", diabetic = T, hypertensive = T)

calculate_uacr_from_upcr <- function(PCR, gender, 
                                     diabetic=NA,
                                     hypertensive=NA,
                                     PCR_units = "mg/g") {
  
  # validate gender
  gender_query = tolower(handle_sex(gender))
  female = gender_query =="female"
  
  # convert PCR units if needed (normal <150 mg/g)
  PCR = ifelse(PCR_units == "mg/g", PCR,
               ifelse(PCR_units %in% c("mg/mg", "ug/g"), PCR*1000, NA))
  
  # from PCR
  pACR = exp(5.2659 + 0.2934*log(min(PCR/50, 1)) +
               1.5643*log(max(min(PCR/500, 1), 0.1)) +
               1.1109*log(max(PCR/500, 1))-
               ifelse(female, 0.0773, 0) +
               ifelse(diabetic, 0.0797, 0) +
               ifelse(hypertensive, 0.1265, 0))
  
  pErr = sqrt(
    exp(-2.0664 + 0.1658*log(min(pACR/30, 1))-
          0.4599*log(max(min(pACR/300, 1), 0.1))-
          0.3084*log(max(pACR/300, 1)) +
          ifelse(female, 0.0847, 0) -
          ifelse(diabetic, 0.2553, 0) -
          ifelse(hypertensive, 0.2299, 0)))
  
  
  res = data.frame("pACR"=pACR,
                   "pACR_err"=pErr)
  return(res)
}

