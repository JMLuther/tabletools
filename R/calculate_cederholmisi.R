#' Cederholm Index Calculation (from standard 75g OGTT)
#'
#' This function calculates the Cederholm Index using glucose and insulin 
#' sampled during a standard 75g oral glucose tolerance test.
#' 
#' Standard timepoints are 0, 30, 60, 90, and 120 min.
#' Note: insulin unit conversion may differ differ depending on assay. 
#' Insulin (pmol/l) = insulin (uU/ml)*6 
#' 
#' The formula used for this calculation is described in \href{https://pubmed.ncbi.nlm.nih.gov/2261853/}{Cederholm et al.}:
#' \deqn{\frac{75000 + (Glucose_{0} - Glucose_{120}) \cdot 1.15 \cdot 180 \cdot 0.19 \cdot Weight(kg)} 
#' {120 \cdot log(Insulin_{mean}) \cdot Glucose_{mean}}
#' }
#' 
#'  
#' @param time  a column name (unquoted) indicating time values (in minutes)
#' @param glucose a column name (unquoted) storing glucose values (in mg/dL)
#' @param insulin a column name (unquoted) storing insulin values (in uU/mL)
#' @param time_units if units are not in "min", can indicate here for unit conversion (options "min" or "hr")
#' @param glucose_units if units are not in "mg/dl", can indicate here for unit conversion (options "mg/dl" or "mmol/l") 
#' @param insulin_units if units are not in "uU/ml", can indicate here for unit conversion (options "uU/ml" or "pmol/l")
#'
#' @return Insulinogenic index as a single value 
#' @export
#' @examples 
#' # individual objects for each item
#' time=c(0, 30, 60, 90, 120)              # minutes
#' glucose=c(93, 129, 178, 164, 97)        # mg/dL
#' insulin=c(12.8, 30.7, 68.5, 74.1, 44.0) # uU/mL
#' calculate_cederholm_isi(time, glucose, insulin, weight=70) # 11.80
#' 
#' # handling data stored in a dataframe
#' ogtt1 <- data.frame(time=c(0, 30, 60, 90, 120),              # minutes
#'                     glucose=c(93, 129, 178, 164, 97),        # mg/dL
#'                     insulin=c(12.8, 30.7, 68.5, 74.1, 44.0)) # uU/mL
#' calculate_cederholm_isi(ogtt1$time, ogtt1$glucose, ogtt1$insulin, weight=70) # 11.79529
#' 
#' 
#' 
#' # Convert units
#' ogtt5 <- data.frame(time = c(0,0.5,1,1.5,2), # time in hours
#'                     glucose = c(5.167, 7.167, 9.889, 9.111, 5.3889), # glucose in mmol/l
#'                     insulin = c(76.8,184.2,411,444.6,264)) # insulin in pmol/l
#' 
#' calculate_cederholm_isi(time =  ogtt5$time,
#'                         glucose = ogtt5$glucose,
#'                         insulin = ogtt5$insulin,
#'                         weight=154, weight_units="lbs",
#'                         time_units = "hr", insulin_units = "pmol/l", glucose_units = "mmol/l")

calculate_cederholm_isi <- function(time, glucose, insulin, weight, time_units = "min", 
                            glucose_units = "mg/dl", insulin_units = "uU/ml",
                            weight_units = "kg") {
  
  if (any(is.na(time)) | any(is.na(glucose)) | any(is.na(insulin))) {
    rlang::warn("Check for missing values in time, glucose, and insulin")
    return(NA_real_)}
  
  if (any(!is.numeric(time)) | any(!is.numeric(glucose)) | any(!is.numeric(insulin))) {
    rlang::warn("non-numeric values in time, glucose, or insulin")
    return(NA_real_)}
  
  if (!all(order(time) == seq_along(time))) {
    rlang::warn("Time values must be ordered from 0 to end") # check proper time order
    return(NA_real_)}
  
  # convert units; 
  time = convert_time_to_min(time, time_units)
  glucose = convert_glucose_to_mM(glucose, glucose_units)
  insulin = convert_insulin_to_uU_ml(insulin, insulin_units)
  weight_kg = convert_weight_to_kg(weight, weight_units)
  
  ind0 = which(time==0)
  g0 = glucose[ind0]
  ins0 = insulin[ind0]

  ind120 = which(time==120)
  g120 = glucose[ind120]
  ins120 = insulin[ind120]
  time_max = max(time)
  g_bar <- sfsmisc::integrate.xy(time, glucose, use.spline = FALSE)/time_max 
  ins_bar <- sfsmisc::integrate.xy(time, insulin,use.spline = FALSE)/time_max
  
    return( (75000 + (g0 - g120) * 1.15 *180 *0.19 *weight_kg) / (120*log(ins_bar) * g_bar))
}
