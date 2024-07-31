#' H2PEF calculator
#' Returns the H2FPEF
#' 
#' Estimated probability of HFpEF based on a validated risk calculator \href{https://pubmed.ncbi.nlm.nih.gov/29792299/}{(Reddy 2018)}.
#' This calculator uses the continuous model provided online, downloadable as an XLS file,
#' rather than the point-based estimate. Results of this function have been checked against this online calculator.  
#'
#'
#' @param age Age (years)
#' @param bmi BMI, kg/m^2
#' @param pa_pressure estimated Pulmonary Artery Pressure (mm/hg) 
#' @param filling_pressure (Doppler echo E/e') 
#' @param afib Atrial Fibrillation (T/F)
#'
#' @return H2PEF probability (%) 
#' @export
#'
#' @examples
#' calculate_h2fpef(age=45, bmi=31, filling_pressure=9, pa_pressure = 35, afib = F)
#' calculate_h2fpef(age=60, bmi=25, filling_pressure=5, pa_pressure = 12, afib = T)
#' calculate_h2fpef(age=51, bmi=30, filling_pressure=10, pa_pressure = 45, afib = T)


calculate_h2fpef <- function(age, bmi, pa_pressure, filling_pressure, afib) {
  log_odds = -9.19174463966566 + 0.0451129471272832*age + 0.130730156015681*bmi + 0.0858634402456586*filling_pressure + 0.051963758732548*pa_pressure + 1.69968057294513*afib
  # log_odds = -9.1917 + 0.0451*age +0.1307*bmi + 0.0859*filling_pressure + 0.0520*pa_pressure +1.6997*afib
  odds = exp(log_odds)
  prob = 100*odds/(1+odds)
  return(prob)
}
