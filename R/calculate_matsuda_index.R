#' Matsuda-Defronzo Index Calculation (from standard 75g OGTT)
#'
#' This function calculates the Matsuda index using glucose and insulin 
#' sampled during a standard 75g oral glucose tolerance test.
#' Results can be checked using the \href{http://mmatsuda.diabetes-smc.jp/MIndex.html}{Matsuda online calculator}. 
#' 
#' Standard timepoints are 0, 30, 60, 90, and 120 min.
#' Note: insulin unit conversion may differ differ depending on assay. 
#' Insulin (pmol/l) = insulin (uU/ml)*6 
#' 
#' `calculate_matsuda_index_df()` requires a dataframe input with time, glucose, insulin columns. 
#' `calculate_matsuda_index()` accepts 3 separate vectors for time, glucose, insulin. 
#'  
#' @param time  a column name (unquoted) indicating time values (in minutes)
#' @param glucose a column name (unquoted) storing glucose values (in mg/dL)
#' @param insulin a column name (unquoted) storing insulin values (in uU/mL)
#' @param time_units if units are not in "min", can indicate here for unit conversion (options "min" or "hr")
#' @param glucose_units if units are not in "mg/dl", can indicate here for unit conversion (options "mg/dl" or "mmol/l") 
#' @param insulin_units if units are not in "uU/ml", can indicate here for unit conversion (options "uU/ml" or "pmol/l")
#'
#' @return Matsuda index as a single value 
#' @export
#' @import sfsmisc
#' @examples 
#' individual objects for each item
#' time=c(0, 30, 60, 90, 120)              # minutes
#' glucose=c(93, 129, 178, 164, 97)        # mg/dL
#' insulin=c(12.8, 30.7, 68.5, 74.1, 44.0) # uU/mL
#' calculate_matsuda_index(time, glucose, insulin) # 3.43125
#' 
#' # handling data stored in a dataframe
#' ogtt1 <- data.frame(time=c(0, 30, 60, 90, 120),              # minutes
#'                     glucose=c(93, 129, 178, 164, 97),        # mg/dL
#'                     insulin=c(12.8, 30.7, 68.5, 74.1, 44.0)) # uU/mL
#' calculate_matsuda_index(ogtt1$time, ogtt1$glucose, ogtt1$insulin) # 3.43125
#' 
#' 
#' # error if time is out of order
#' ogtt3 <- data.frame(
#'   time=c(0, 60, 30, 90, 120),              # minutes
#'   glucose=c(100, 129, 178, 164, 97),        # mg/dL
#'   insulin=c(12.8, 100, 68.5, 74.1, 44.0)) # uU/mL
#' 
#' calculate_matsuda_index(time =  ogtt3$time,
#'                         glucose = ogtt3$glucose,
#'                         insulin = ogtt3$insulin) # error
#' 
#' # error if missing values
#' ogtt4 <- data.frame(
#'   time=c(0, 60, 30, 90, 120),              # minutes
#'   glucose=c(100, NA, 178, 164, 97),        # mg/dL
#'   insulin=c(12.8, 100, 68.5, 74.1, 44.0)) # uU/mL
#' 
#' calculate_matsuda_index(time =  ogtt4$time,
#'                         glucose = ogtt4$glucose,
#'                         insulin = ogtt4$insulin) # error
#' 
#' # Convert units:
#' ogtt5 <- data.frame(time = c(0,0.5,1,1.5,2), # time in hours
#'                     glucose = c(5.167, 7.167, 9.889, 9.111, 5.3889), # glucose in mmol/l
#'                     insulin = c(76.8,184.2,411,444.6,264)) # insulin in pmol/l
#' 
#' calculate_matsuda_index(time =  ogtt5$time,
#'                         glucose = ogtt5$glucose,
#'                         insulin = ogtt5$insulin,
#'                         time_units = "hr", insulin_units = "pmol/l", glucose_units = "mmol/l")

calculate_matsuda_index <- function(time, glucose, insulin, time_units = "min", 
                                    glucose_units = "mg/dl", insulin_units = "uU/ml") {

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
  # doesn't alter the underlying dataframe, only 
  time = convert_time_to_min(time, time_units)
  glucose = convert_glucose_to_mgdl(glucose, glucose_units)
  insulin = convert_insulin_to_uU_ml(insulin, insulin_units)

  ind0 = which(time==0)
  g0 = glucose[ind0]
  ins0 = insulin[ind0]
  time_max = max(time)
  g_bar <- sfsmisc::integrate.xy(time, glucose, use.spline = FALSE)/time_max 
  ins_bar <- sfsmisc::integrate.xy(time, insulin,use.spline = FALSE)/time_max
  return(10000/sqrt(g0*ins0*g_bar*ins_bar))
}
