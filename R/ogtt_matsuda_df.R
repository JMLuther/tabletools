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
#' `matsuda_df()` requires a dataframe input with time, glucose, insulin columns. 
#' `matsuda()` accepts 3 separate vectors with time, glucose, insulin. 
#'  
#' 
#' @param df A long-format data frame
#' @param timeCol a column name (unquoted) indicating time values (in minutes)
#' @param glucoseCol a column name (unquoted) storing glucose values (in mg/dL)
#' @param insulinCol a column name (unquoted) storing insulin values (in uU/mL)
#' @param time_units if units are not in "min", can indicate here for unit conversion (options "min" or "hr")
#' @param glucose_units if units are not in "mg/dl", can indicate here for unit conversion (options "mg/dl" or "mmol/l") 
#' @param insulin_units if units are not in "uU/ml", can indicate here for unit conversion (options "uU/ml" or "pmol/l")
#'
#' @return Matsuda index as a single value 
#' @export
#' @examples 
#' # A long-format dataframe with a single subject
#' ogtt1 <- data.frame(
#'   time=c(0, 30, 60, 90, 120),              # minutes
#'   glucose=c(93, 129, 178, 164, 97),        # mg/dL
#'   insulin=c(12.8, 30.7, 68.5, 74.1, 44.0)) # uU/mL
#'   
#' # dataframe with variable columns appropriately named
#'   matsuda_df(ogtt1) # works because columns are default names
#'   matsuda_df(ogtt1, time, glucose, insulin)
#'   
#' # Need to specify column names if named differently
#' ogtt2 <- data.frame(
#'     t=c(0, 30, 60, 90, 120),              # minutes
#'     gluc=c(93, 129, 178, 164, 97),        # mg/dL
#'     ins=c(12.8, 30.7, 68.5, 74.1, 44.0)) # uU/mL
#' matsuda_df(ogtt2)
#' matsuda_df(ogtt2, t, gluc, ins) # works by positional matching
#' matsuda_df(ogtt2, timeCol = t, glucoseCol =  gluc, insulinCol =  ins)
#' 
#' # # error if time data is out of order
#' ogtt3 <- data.frame(
#'   time=c(0, 60, 30, 90, 120),              # minutes
#'   glucose=c(100, 129, 178, 164, 97),        # mg/dL
#'   insulin=c(12.8, 100, 68.5, 74.1, 44.0)) # uU/mL
#'   
#'   matsuda_df(ogtt3, time, glucose, insulin)
#'   
#' # error if missing values
#' ogtt4 <- data.frame(
#'   time=c(0, 60, 30, 90, 120),              # minutes
#'   glucose=c(100, NA, 178, 164, 97),        # mg/dL
#'   insulin=c(12.8, 100, 68.5, 74.1, 44.0)) # uU/mL
#'   
#'   matsuda_df(ogtt4, time, glucose, insulin)
#' 
#' # dataframe with differently named variable columns
#' # Convert units:
#' ogtt5 <- data.frame(time = c(0,0.5,1,1.5,2), # time in hours
#'                     glucose = c(5.167, 7.167, 9.889, 9.111, 5.3889), # glucose in mmol/l
#'                     insulin = c(76.8,184.2,411,444.6,264)) # insulin in pmol/l
#' 
#' # you don't have to specify units, if using default values
#' matsuda_df(ogtt1, time, glucose, insulin,
#'            insulin_units = "uU/ml", glucose_units = "md/dl") # default values
#' # if not default, specify units for proper conversinon:
#' matsuda_df(ogtt5, time, glucose, insulin,
#'            time_units = "hr", insulin_units = "pmol/l", glucose_units = "mmol/l")
#' 
#' matsuda_df(ogtt5, time_units = "hr", insulin_units = "pmol/l", glucose_units = "mmol/l")

matsuda_df <- function(df, timeCol=time, glucoseCol=glucose, insulinCol=insulin, 
                       time_units = "min", glucose_units = "mg/dl", insulin_units = "uU/ml") {
  timeCol <- deparse(substitute(timeCol))  # accepts unquoted Var/Col name
  glucoseCol <- deparse(substitute(glucoseCol))   # accepts unquoted Var/Col name
  insulinCol <- deparse(substitute(insulinCol))  # accepts unquoted Var/Col name
  if (any(is.na(df[[timeCol]])) | any(is.na(df[[glucoseCol]])) | any(is.na(df[[insulinCol]]))) {
    stop("Check for missing values in time, glucose, and insulin")} 
  if (!all(order(df[[timeCol]]) == seq_along(df[[timeCol]]))) {
    stop("Time values must be ordered from 0 to end")} # check proper time order
  # convert time units to min
  if (time_units == "min") {
    df[[timeCol]] = df[[timeCol]]
  } else if (time_units == "hr") {
    df[[timeCol]] = df[[timeCol]]*60}

  # convert time glucose to min
  if (glucose_units == "mg/dl") {
    df[[glucoseCol]] = df[[glucoseCol]]
  } else if (glucose_units == "mmol/l") {
    df[[glucoseCol]] = df[[glucoseCol]]*18}
    
  # convert insulin units to uU/ml
  if (insulin_units == "uU/ml") {
    df[[insulinCol]] = df[[insulinCol]]
  } else if (insulin_units == "pmol/l") {
    df[[insulinCol]] = df[[insulinCol]]/6} # VUMC conversion units in DRTC lab
    
  ind0 <- which(df[[timeCol]]==0)
  g0 <- df[[glucoseCol]][ind0]
  ins0 <- df[[insulinCol]][ind0]
  time_len <- max(df[[timeCol]])
  g_bar <- sfsmisc::integrate.xy(df[[timeCol]], df[[glucoseCol]], use.spline = FALSE)/time_len
  ins_bar <- sfsmisc::integrate.xy(df[[timeCol]], df[[insulinCol]], use.spline = FALSE)/time_len
  return(10000/sqrt(g0*ins0*g_bar*ins_bar))
}

