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
#' @importFrom sfsmisc integrate.xy 
#' @examples 
#' library(dplyr)
#' # A long-format dataframe with a single subject
#' ogtt1 <- data.frame(
#'   time=c(0, 30, 60, 90, 120),              # minutes
#'    glucose=c(93, 129, 178, 164, 97),        # mg/dL
#'    insulin=c(12.8, 30.7, 68.5, 74.1, 44.0)) # uU/mL
#'    
#'  # dataframe with variable columns appropriately named
#'    calculate_matsuda_index_df(ogtt1) # works because columns are default names
#'    calculate_matsuda_index_df(ogtt1, time, glucose, insulin)
#'    
#'  # Specify column names if named differently
#'  ogtt2 <- data.frame(
#'      t=c(0, 30, 60, 90, 120),              # minutes
#'      gluc=c(93, 129, 178, 164, 97),        # mg/dL
#'      ins=c(12.8, 30.7, 68.5, 74.1, 44.0)) # uU/mL
#'  
#'  # dataframe with differently named variable columns
#'  # Convert units
#'  ogtt5 <- data.frame(time = c(0,0.5,1,1.5,2), # time in hours
#'                      glucose = c(5.167, 7.167, 9.889, 9.111, 5.3889), # glucose in mmol/l
#'                      insulin = c(76.8,184.2,411,444.6,264)) # insulin in pmol/l
#'  
#'  # you don't have to specify units, if using default values
#'  calculate_matsuda_index_df(ogtt1, time, glucose, insulin,
#'             insulin_units = "uU/ml", glucose_units = "mg/dl") # default values
#'  # if not default, specify units for proper conversino
#'  calculate_matsuda_index_df(ogtt5, time, glucose, insulin,
#'             time_units = "hr", insulin_units = "pmol/l", glucose_units = "mmol/l")
#'  
#'  calculate_matsuda_index_df(ogtt5, time_units = "hr", insulin_units = "pmol/l", glucose_units = "mmol/l")
#'  
#' # Handle multiple OGTT data frames
#' # These all need to be structured the same as a nested list.
#' nested_df <- dplyr::tibble(id = 1:3,
#'                            data = list(ogtt1, ogtt1, ogtt1)) # same data in this case
#' dplyr::mutate(nested_df, matsuda= purrr::map_dbl(data, ~calculate_matsuda_index_df(.x)))
#'
calculate_matsuda_index_df <- function(df, timeCol=time, glucoseCol=glucose, insulinCol=insulin,
                                       time_units = "min", glucose_units = "mg/dl", insulin_units = "uU/ml") {
  timeCol <- deparse(substitute(timeCol))  # accepts unquoted Var/Col name
  glucoseCol <- deparse(substitute(glucoseCol))   # accepts unquoted Var/Col name
  insulinCol <- deparse(substitute(insulinCol))  # accepts unquoted Var/Col name
  
  if (any(is.na(df[[timeCol]])) | any(is.na(df[[glucoseCol]])) | any(is.na(df[[insulinCol]]))) {
    rlang::warn("Check for missing values in time, glucose, and insulin")
    return(NA_real_)}
  
  if (!all(order(df[[timeCol]]) == seq_along(df[[timeCol]]))) {
    rlang::warn("Time values must be ordered from 0 to end") # check proper time order
    return(NA_real_)}
  
  # convert units; 
  df[[timeCol]] = convert_time_to_min(df[[timeCol]], time_units)
  df[[glucoseCol]] = convert_glucose_to_mgdl(df[[glucoseCol]], glucose_units)
  df[[insulinCol]] = convert_insulin_to_uU_ml(df[[insulinCol]], insulin_units)
  
  ind0 <- which(df[[timeCol]]==0)
  g0 <- df[[glucoseCol]][ind0]
  ins0 <- df[[insulinCol]][ind0]
  time_len <- max(df[[timeCol]])
  g_bar <- sfsmisc::integrate.xy(df[[timeCol]], df[[glucoseCol]], use.spline = FALSE)/time_len
  ins_bar <- sfsmisc::integrate.xy(df[[timeCol]], df[[insulinCol]], use.spline = FALSE)/time_len
  return(10000/sqrt(g0*ins0*g_bar*ins_bar))
}
