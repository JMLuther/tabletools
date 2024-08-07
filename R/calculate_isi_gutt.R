#' Gutt Insulin Sensitivity Index Calculation (from standard 75g OGTT)
#'
#' This function calculates the Gutt Insulin Sensitivity Index using glucose and
#' insulin sampled during a standard 75g oral glucose tolerance test. The Gutt
#' ISI is a simplification of the Cederholm index, using only the 0 and 120min
#' values as described in \href{https://pubmed.ncbi.nlm.nih.gov/10741566/}{Gutt el
#' al}.
#'
#' Standard timepoints are 0, 30, 60, 90, and 120 min. Note: insulin unit
#' conversion may differ differ depending on assay. Insulin (pmol/l) = insulin
#' (uU/ml)*6
#'
#' `calculate_isi_gutt()` accepts 3 separate vectors for time, glucose, insulin.
#'
#' @param time  a column name (unquoted) indicating time values (in minutes)
#' @param glucose a column name (unquoted) storing glucose values (in mg/dL)
#' @param insulin a column name (unquoted) storing insulin values (in uU/mL)
#' @param bmi body mass index (\eqn{kg/m^2})
#' @param time_units if units are not in "min", can indicate here for unit
#'   conversion (options "min" or "hr")
#' @param glucose_units if units are not in "mg/dl", can indicate here for unit
#'   conversion (options "mg/dl" or "mmol/l")
#' @param insulin_units if units are not in "uU/ml", can indicate here for unit
#'   conversion (options "uU/ml" or "pmol/l")
#'
#' @return Insulin Sensitivity-Gutt (mL/kg/min) as a single value
#' @export
#' @examples
#' # individual objects for each item
#' time=c(0, 30, 60, 90, 120)              # minutes
#' glucose=c(93, 129, 178, 164, 97)        # mg/dL
#' insulin=c(12.8, 30.7, 68.5, 74.1, 44.0) # uU/mL
#' calculate_isi_gutt(time, glucose, insulin, bmi=30)
#'
#' # handling data stored in a dataframe
#' ogtt1 <- data.frame(time=c(0, 30, 60, 90, 120),              # minutes
#'                     glucose=c(93, 129, 178, 164, 97),        # mg/dL
#'                     insulin=c(12.8, 30.7, 68.5, 74.1, 44.0)) # uU/mL
#' calculate_isi_gutt(ogtt1$time, ogtt1$glucose, ogtt1$insulin, bmi=30)
#'
#' # example from Gutch et al 2015
#' ogtt2 <- data.frame(time=c(0, 30, 60, 90, 120),              # minutes
#'                     glucose=c(100, 160, 160, 160, 140),      # mg/dL
#'                     insulin=c(5, 10, 10, 10, 5))             # uU/mL
#' calculate_isi_gutt(ogtt2$time, ogtt2$glucose, ogtt2$insulin, bmi=30)
#' calculate_matsuda_index(ogtt2$time, ogtt2$glucose, ogtt2$insulin)
#'
#' # Convert units
#' ogtt5 <- data.frame(time = c(0,0.5,1,1.5,2), # time in hours
#'                     glucose = c(5.167, 7.167, 9.889, 9.111, 5.3889), # glucose in mmol/l
#'                     insulin = c(76.8,184.2,411,444.6,264)) # insulin in pmol/l
#'
#' calculate_isi_gutt(time =  ogtt5$time,
#'                         glucose = ogtt5$glucose,
#'                         insulin = ogtt5$insulin, bmi=30,
#'                         time_units = "hr", insulin_units = "pmol/l", glucose_units = "mmol/l")

calculate_isi_gutt <- function(time, glucose, insulin, bmi, time_units = "min", 
                                    glucose_units = "mg/dl", insulin_units = "uU/ml") {
  
  if (any(!is.numeric(time)) | any(!is.numeric(glucose)) | any(!is.numeric(insulin))) {
    rlang::warn("non-numeric values in time, glucose, or insulin")
    return(NA_real_)}

  # convert units; 
  time = convert_time_to_min(time, time_units)
  glucose = convert_glucose_to_mgdl(glucose, glucose_units)
  insulin = convert_insulin_to_uU_ml(insulin, insulin_units)
  
  ind0 = which(time==0)
  g0 = glucose[ind0]
  ins0 = insulin[ind0]

  ind120 = which(time==120)
  g120 = glucose[ind120]
  ins120 = insulin[ind120]

  if (any(is.na(ins120)) | any(is.na(ins0)) | any(is.na(g120))) {
    rlang::warn("Check for missing values in Age, BMI, or Insulin t=120")
    return(NA_real_)}
  
  isi_gutt = (75000 + (g0-g120)*0.19*bmi)/(120*log((ins0+ins120)/2)*((g0+g120)/2))
  
    return(isi_gutt)
}


