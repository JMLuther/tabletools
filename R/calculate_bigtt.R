#' OGTT BIGTT estimates of beta cell function and insulin sensitivity
#'
#' This function calculates the BIGTT estimates of insulin sensitivity and beta
#' cell function using glucose and insulin sampled during a standard 75g oral
#' glucose tolerance test. Also requires BMI and gender.
#'
#' Two estimates of SI are provided using either 0,30,120 or 0,60,120 values.
#' The calculations are described in
#' \href{https://pubmed.ncbi.nlm.nih.gov/17259491/}{Hansen et al}. The "full"
#' version is not included here due to using non-standard time points that I
#' have not collected.
#'
#' Standard timepoints are 0, 30, 60, 90, and 120 min. Note: insulin unit
#' conversion may differ differ depending on assay. Insulin (pmol/l) = insulin
#' (uU/ml)*6
#'
#' `calculate_bigtt()` accepts 3 separate vectors for time, glucose,
#' insulin and values for gender and BMI.
#'
#' @param time  a column name (unquoted) indicating time values (in minutes)
#' @param glucose a column name (unquoted) storing glucose values (in mg/dL)
#' @param insulin a column name (unquoted) storing insulin values (in uU/mL)
#' @param gender Gender (M/F)
#' @param bmi body mass index (\eqn{kg/m^2})
#' @param time_units if units are not in "min", can indicate here for unit
#'   conversion (options "min" or "hr")
#' @param glucose_units if units are not in "mg/dl", can indicate here for unit
#'   conversion (options "mg/dl" or "mmol/l")
#' @param insulin_units if units are not in "uU/ml", can indicate here for unit
#'   conversion (options "uU/ml" or "pmol/l")
#'
#' @return Dataframe of BIGTT Si and AIR estimates
#' @export
#'
#' @examples
#' time=c(0, 30, 60, 90, 120)              # minutes
#' glucose=c(93, 129, 178, 164, 97)        # mg/dL
#' insulin=c(12.8, 30.7, 68.5, 74.1, 44.0) # uU/mL
#' calculate_bigtt(time, glucose, insulin, gender="female", bmi=30)
#' calculate_bigtt(time, glucose, insulin, gender="Male", bmi=30) 
#' calculate_bigtt(time, glucose, insulin, gender="Male") 
#' calculate_bigtt(time, glucose, insulin, bmi=30) 

calculate_bigtt <- function(time, glucose, insulin, gender=NA, bmi=NA,
                                time_units = "min", 
                                   glucose_units = "mg/dl", insulin_units = "uU/ml") {
  
  if (any(!is.numeric(time)) | any(!is.numeric(glucose)) | any(!is.numeric(insulin))) {
    rlang::warn("non-numeric values in time, glucose, insulin")
    return(NA_real_)}

  # convert units; 
  time = convert_time_to_min(time, time_units)
  glucose = convert_glucose_to_mM(glucose, glucose_units)
  insulin = convert_insulin_to_pM(insulin, insulin_units)

  ind0 = which(time==0)
  g0 = glucose[ind0]
  ins0 = insulin[ind0]
  
  ind30 = which(time==30)
  g30 = glucose[ind30]
  ins30 = insulin[ind30]

  ind60 = which(time==60)
  g60 = glucose[ind60]
  ins60 = insulin[ind60]
  
  ind120 = which(time==120)
  g120 = glucose[ind120]
  ins120 = insulin[ind120]
  
  # if (any(is.na(bmi)) | any(is.na(gender)) ) {
  #   rlang::warn("Check for missing values in Age, BMI, or Insulin t=120")
  #   return(NA_real_)}
  
  # sex = switch(gender, 
  #              female=0, Female=0,"F"=0,
  #              male=1, Male=1, "M"=1)
  sex = switch(as.character(gender), 
               female=0, Female=0,"F"=0,
               male=1, Male=1, "M"=1,
               "NA"=NA_integer_)
  
  bigtt_air_0_30_120 = exp(8.20+ 0.00178*ins0 + 0.00168*ins30 - 0.000383*ins120 - 0.314*g0 - 0.109*g30 + 0.0781*g120 + 0.180*sex + 0.032*bmi)
  bigtt_air_0_60_120 = exp(8.19+0.00339*ins0 + 0.00152*ins60 - 0.000959*ins120 - 0.389*g0 - 0.142*g60 + 0.164*g120 + 0.256*sex + 0.038*bmi)

  bigtt_si_0_30_120 = exp(4.90-0.00402*ins0 - 0.000556*ins30 - 0.00127*ins120- 0.152*g0 - 0.00871*g30 - 0.0373*g120 - 0.145*sex - 0.0376*bmi)
  bigtt_si_0_60_120 = exp(4.62-0.00385*ins0 - 0.000917*ins60 - 0.000760*ins120- 0.0551*g0 - 0.0178*g60 - 0.0524*g120 - 0.144*sex - 0.0380*bmi)
  # bigtt_air_0_60_120 = exp(8.20+ 0.00178*ins0 + 0.00168*ins60 - 0.000383*ins120 - 0.314*g0 - 0.109*g60 + 0.0781*g120 + 0.180*sex + 0.032*bmi)
  
  
  return(data.frame("bigtt_air_0_30_120"=bigtt_air_0_30_120, "bigtt_si_0_30_120"=bigtt_si_0_30_120,
           "bigtt_air_0_60_120"=bigtt_air_0_60_120, "bigtt_si_0_60_120"=bigtt_si_0_60_120))
}

