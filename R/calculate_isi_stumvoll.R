#' Stumvoll Insulin Sensitivity Index Calculation (from standard 75g OGTT)
#'
#' This function calculates the Stumvoll Insulin Sensitivity Index using glucose
#' and insulin sampled during a standard 75g oral glucose tolerance test.
#' Methods described in
#' \href{https://pubmed.ncbi.nlm.nih.gov/10868854/}{Stumvoll et al.}
#'
#' Standard timepoints are 0, 30, 60, 90, and 120 min. Note: insulin unit
#' conversion may differ differ depending on assay. Insulin (pmol/l) = insulin
#' (uU/ml)*6
#'
#' `calculate_isi_stumvoll()` accepts 3 separate vectors for time, glucose,
#' insulin. If the demographic `calculate_isi_stumvoll_dem()` version is used,
#' BMI and age are required. Note the demographic version only utilizes Insulin
#' at t=120 and dose not incorporate glucose.
#'
#' @param time  a column name (unquoted) indicating time values (in minutes)
#' @param glucose a column name (unquoted) storing glucose values (in mg/dL)
#' @param insulin a column name (unquoted) storing insulin values (in uU/mL)
#' @param bmi body mass index (\eqn{kg/m^2})
#' @param age age (years)
#' @param time_units if units are not in "min", can indicate here for unit
#'   conversion (options "min" or "hr")
#' @param glucose_units if units are not in "mg/dl", can indicate here for unit
#'   conversion (options "mg/dl" or "mmol/l")
#' @param insulin_units if units are not in "uU/ml", can indicate here for unit
#'   conversion (options "uU/ml" or "pmol/l")
#'
#' @return Stumvoll Insulin Sensitivity Index as a single value
#' @export
#' @examples
#' # individual objects for each item
#' time=c(0, 30, 60, 90, 120)              # minutes
#' glucose=c(93, 129, 178, 164, 97)        # mg/dL
#' insulin=c(12.8, 30.7, 68.5, 74.1, 44.0) # uU/mL
#' calculate_isi_stumvoll(time, glucose, insulin) # 11.80
#'
#' # handling data stored in a dataframe
#' ogtt1 <- data.frame(time=c(0, 30, 60, 90, 120),              # minutes
#'                     glucose=c(93, 129, 178, 164, 97),        # mg/dL
#'                     insulin=c(12.8, 30.7, 68.5, 74.1, 44.0)) # uU/mL
#' calculate_isi_stumvoll(ogtt1$time, ogtt1$glucose, ogtt1$insulin) #
#'
#' # example from Gutch et al 2015
#' ogtt2 <- data.frame(time=c(0, 30, 60, 90, 120),              # minutes
#'                     glucose=c(100, 160, 160, 160, 140),      # mg/dL
#'                     insulin=c(5, 10, 10, 10, 5))             # uU/mL
#' calculate_isi_stumvoll(ogtt2$time, ogtt2$glucose, ogtt2$insulin) #
#' calculate_matsuda_index(ogtt2$time, ogtt2$glucose, ogtt2$insulin)
#'
#' # Convert units
#' ogtt5 <- data.frame(time = c(0,0.5,1,1.5,2), # time in hours
#'                     glucose = c(5.167, 7.167, 9.889, 9.111, 5.3889), # glucose in mmol/l
#'                     insulin = c(76.8,184.2,411,444.6,264)) # insulin in pmol/l
#'
#' calculate_isi_stumvoll(time =  ogtt5$time,
#'                         glucose = ogtt5$glucose,
#'                         insulin = ogtt5$insulin,
#'                         time_units = "hr", insulin_units = "pmol/l", glucose_units = "mmol/l")

calculate_isi_stumvoll <- function(time, glucose, insulin, time_units = "min", 
                                   glucose_units = "mg/dl", insulin_units = "uU/ml") {
  
  if (any(!is.numeric(time)) | any(!is.numeric(glucose)) | any(!is.numeric(insulin))) {
    rlang::warn("non-numeric values in time, glucose, or insulin")
    return(NA_real_)}
  
  # convert units; 
  time = convert_time_to_min(time, time_units)
  glucose = convert_glucose_to_mM(glucose, glucose_units)
  insulin = convert_insulin_to_pM(insulin, insulin_units)
  
  ind0 = which(time==0)
  g0 = glucose[ind0]
  ins0 = insulin[ind0]
  
  ind120 = which(time==120)
  g120 = glucose[ind120]
  ins120 = insulin[ind120]
  
  if (any(is.na(ins120)) | any(is.na(ins0)) | any(is.na(g120))) {
    rlang::warn("Check for missing values in Age, BMI, or Insulin t=120")
    return(NA_real_)}
  
  isi_stumvoll = 0.156-0.0000459*ins120 - 0.000321*ins0 - 0.0054*g120
  
  return(isi_stumvoll)
}

