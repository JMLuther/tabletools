#' Caumo OGTT Insulin Sensitivity Index Calculation (from standard 75g OGTT)
#'
#' This function calculates the Caumo Insulin Sensitivity Index using glucose and insulin 
#' sampled during a standard 75g oral glucose tolerance test.
#' 
#' Standard timepoints are 0, 30, 60, 90, and 120 min.
#' Note: insulin unit conversion may differ differ depending on assay. 
#' Insulin (pmol/l) = insulin (uU/ml)*6 
#' 
#' `calculate_caumo_si()` accepts 3 separate vectors for time, glucose, insulin. Other options include f (absorption fraction), and D (glucose dose)
#' 
#' Uses the formula detailed in \href{https://pubmed.ncbi.nlm.nih.gov/11095485/}{Caumo et al}:
#' \deqn{\frac{f \cdot D_{oral} \cdot \frac{AUC[\Delta g(t)/g(t)]} {AUC[\Delta g(t)]} - GE \cdot AUC[\Delta g(t)/g(t)]}
#' {AUC[\Delta i(t)]}
#' }
#' Where g(t) is glucose and i(t) is insulin during the time course. GE is assumed to be 0.024 \eqn{dL \cdot kg^{-1} \cdot min^{-1}}, f=0.8, and D = 75g (standard OGTT glucose dose). The values for f and D can be changed if desired.
#'  
#' @param time  a column name (unquoted) indicating time values (in minutes); first value assumed to be t=0
#' @param glucose a column name (unquoted) storing glucose values (in mg/dL)
#' @param insulin a column name (unquoted) storing insulin values (in uU/mL)
#' @param weight Body weight, in kg
#' @param time_units if units are not in "min", can indicate here for unit conversion (options "min" or "hr")
#' @param glucose_units if units are not in "mg/dl", can indicate here for unit conversion (options "mg/dl" or "mmol/l") 
#' @param insulin_units if units are not in "uU/ml", can indicate here for unit conversion (options "uU/ml" or "pmol/l")
#' @param weight_units if units are not in "kg", can indicate here for unit conversion (options "lbs" or "g")
#' @param f Fraction of Glucose absorbed (default 0.8)
#' @param dose_glucose Glucose Dose, in grams (default to 75g, standard OGTT)
#' @md
#'
#' @return Caumo Insulin Sensitivity Index as a single value (10^5 dL/kg/min per pmol/L) 
#' @export
#' @examples 
#' # individual objects for each item
#' time=c(0, 30, 60, 90, 120)              # minutes
#' glucose=c(93, 129, 178, 164, 97)        # mg/dL
#' insulin=c(12.8, 30.7, 68.5, 74.1, 44.0) # uU/mL
#' calculate_caumo_si(time, glucose, insulin, weight=70) # 9.9
#' 
#' # handling data stored in a dataframe
#' ogtt1 <- data.frame(time=c(0, 30, 60, 90, 120),              # minutes
#'                     glucose=c(93, 129, 178, 164, 97),        # mg/dL
#'                     insulin=c(12.8, 30.7, 68.5, 74.1, 44.0)) # uU/mL
#' calculate_caumo_si(ogtt1$time, ogtt1$glucose, ogtt1$insulin, weight=70)
#' 
#' # Example MTT values from Caumo JCEM 2000
#' ogtt_240 <- 
#' data.frame(
#'   time = c(0L,10L,20L,30L,40L,50L,60L,75L, # min
#'            90L,120L,150L,180L,210L,240L),
#'   glucose = c(91.20261837,90.66992397,108.3909704, # mg/dL
#'               139.8847729,152.5975273,143.3663292,129.5222883,
#'               112.3147056,101.7628589,89.75299304,89.80197787,86.49105416,
#'               86.07894263,85.66752014),
#'   insulin = c(9.999828359,11.48044146,30.08866995, # uU/mL
#'               67.20760028,80.39017525,92.0833834,88.67015671,
#'               69.61466504,52.58035392,35.53244881,30.61193594,21.64875303,
#'               12.36703798,9.680575342))
#' with(ogtt_240, calculate_caumo_si(time, glucose, insulin, weight = 70))
#' with(ogtt_240, calculate_caumo_si(time, glucose, insulin, 
#' weight = 154, weight_units="lbs"))
#' with(ogtt_240, calculate_caumo_si(time, glucose, insulin))

calculate_caumo_si <- function(time, glucose, insulin, weight=NULL,
                               f=0.8, dose_glucose=75,
                               time_units = "min", glucose_units = "mg/dl", 
                               insulin_units = "uU/ml", weight_units="kg") {
  
  if (is.null(weight)) {
    rlang::warn("please enter body weight (in kg)")
    return(NA_real_)}
  
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
  glucose = convert_glucose_to_mgdl(glucose, glucose_units)
  insulin = convert_insulin_to_uU_ml(insulin, insulin_units)
  weight  = convert_weight_to_kg(weight, weight_units)
  
  dg = glucose - glucose[1] # dG above basal Glucose
  dg_g = dg/glucose         # 
  auc_dg = sfsmisc::integrate.xy(time, dg, use.spline = FALSE)
  auc_dg_g = sfsmisc::integrate.xy(time, dg_g, use.spline = FALSE)
  di = insulin - insulin[1]
  auc_di = sfsmisc::integrate.xy(time, di, use.spline = FALSE)
  GE = 0.024       # Glucose Effectiveness, dL/kg/min
  f = 0.8          # Fraction of glucose absorbed
  D = dose_glucose*1000/weight # Dose of glucose, in mg/kg (75g standard OGTT)
  
  caumo_si = (f*D*(auc_dg_g/auc_dg) - GE*auc_dg_g)/auc_di*10000

    return(caumo_si)
}


