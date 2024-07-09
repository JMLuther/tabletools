#' H2PEF calculator
#' Returns the H2FPEF
#'
#'
#' @param bmi BMI, kg/m^2
#' @param htn_meds Number of Hypertension medications
#' @param afib Atrial Fibrillation (T/F)
#' @param pa_pressure estimated Pulmonary Artery Pressure (mm/hg) 
#' @param age Age (years)
#' @param filling_pressure (Doppler echo E/e') 
#'
#' @return 
#' @export
#'
#' @examples
#' calculate_h2fpef(bmi=31, htn_meds = 3, afib = F, pa_pressure = 15, age = F, filling_pressure = 6)
#' library(dplyr, purrr)
#' dat=data.frame(bmi=c(20, 31, 50),
#'                htn_meds=c(0,3,3),
#'                afib=c(F,F,T),
#'                pa_pressure=c(15, 35, 36),
#'                age=c(45, 45, 65),
#'                filling_pressure=c(6, 9, 10))
#' 
#' purrr::pmap_df(dat, calculate_h2fpef)
#' dat |> 
#'   mutate(purrr::pmap_df(dat, calculate_h2fpef))


calculate_h2fpef <- function(bmi, htn_meds, afib, pa_pressure, age, filling_pressure) {
  
  H1  = ifelse(bmi>30, 2, 0)
  H2  = ifelse(htn_meds>=2, 1, 0)
  Fib = ifelse(afib, 3, 0)
  P   = ifelse(pa_pressure>35, 1, 0)
  E   = ifelse(age>60, 1, 0)
  Fill= ifelse(filling_pressure>9, 1, 0)

  h2fpef = as.character(sum(H1, H2, Fib, P, E, Fill))
  h2fpfef_t <- c(14,25,40,56,72,84,91,95,98,99)
  names(h2pfef_t) = 0:9

  return(data.frame("h2fpef_score"=as.numeric(h2fpef),
           "h2fpef_probability"=h2pfef_t[[h2fpef]]/100,
           "h2fpef_description"=paste0("H2FPEF score is ", h2fpef, " points. Probability of HFpEF is ", h2pfef_t[[h2fpef]], "%")))
}

