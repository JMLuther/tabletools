#' OGTT Insulin Sensitivity Index Calculation (OGIS; from standard 75g OGTT)
#'
#' This function calculates the OGIS index using glucose and insulin sampled
#' during a standard 75g oral glucose tolerance test.
#'
#' OGIS requires measurements at 0, 90, and 120 minutes. Alternatively the 3hr
#' version 'calculate_isi_ogis_180()' requires valuse at 0, 120, and 180 minutes.

#' can be used. Note: insulin unit conversion may differ differ depending on
#' assay. Insulin (pmol/l) = insulin (uU/ml)*6 Results are adjusted for BSA OGIS
#' is described in \href{https://pubmed.ncbi.nlm.nih.gov/11289482/}{Mari et
#' al.}. An Online calculator is
#' \href{http://webmet.pd.cnr.it/ogis/ogis.php}{available here.}
#'
#' `calculate_isi_ogis()` accepts vectors for time, glucose, insulin, height, and
#' weight.
#'
#' @param time  a vector indicating time values (in minutes)
#' @param glucose a vecto storing glucose values (in mg/dL)
#' @param insulin a vector storing insulin values (in uU/mL)
#' @param height height, in meters (or )
#' @param weight weight, in kg
#' @param time_units if units are not in "min", can indicate here for unit
#'   conversion (options "min" or "hr")
#' @param glucose_units if units are not in "mg/dl", can indicate here for unit
#'   conversion (options "mg/dl" or "mmol/l")
#' @param insulin_units if units are not in "uU/ml", can indicate here for unit
#'   conversion (options "uU/ml" or "pmol/l")
#' @param height_units if units are not in "m", can indicate here for unit
#'   conversion (options "m" or "cm")
#' @param weight_units if units are not in "kg", can indicate here for unit
#'   conversion (options "kg" or "lbs")
#'
#' @return OGIS Index as a single value (\eqn{ml\cdot min^{-1} \cdot m^2})
#' @export
#' @examples
#' # individual objects for each item
#' library(tabletools)
#' time = c(0,90,120)
#' glucose = c(90, 135, 115)
#' insulin = c(15, 150, 0)
#' calculate_isi_ogis(time = time,  # 344.5
#'                glucose = glucose,
#'                insulin = insulin,
#'                height=170, height_units = "cm",
#'                weight = 70, weight_units = "kg")
#' calculate_isi_ogis(time, glucose, insulin, height=1.70, weight = 70)
#' calculate_isi_ogis(time, glucose, insulin,
#'                height=70, height_units = "in",
#'                weight = 154, weight_units = "lbs")
#'
#' time = c(0,1.5,2)
#' glucose = c(5.0, 7.5, 6.39)
#' insulin = c(90, 900, 0)
#' calculate_isi_ogis(time = time, time_units="hrs",  # 345
#'                glucose = glucose, glucose_units = "mmol/l",
#'                insulin = insulin, insulin_units = "pmol/l",
#'                height=170, height_units = "cm",
#'                weight = 70, weight_units = "kg")
#'
#'
#' time = c(0,120, 180)
#' glucose = c(90, 135, 115)
#' insulin = c(15, 150, 0)
#' calculate_isi_ogis_180(time = time,  # 344.5
#'                glucose = glucose,
#'                insulin = insulin,
#'                height=170, height_units = "cm",
#'                weight = 70, weight_units = "kg")

calculate_isi_ogis <- function(time, glucose, insulin, height, weight, 
                           time_units = "min", height_units = "m", weight_units="kg", 
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
  time = convert_time_to_min(time, time_units)
  glucose = convert_glucose_to_mgdl(glucose, glucose_units)
  insulin = convert_insulin_to_uU_ml(insulin, insulin_units)
  weight_kg = convert_weight_to_kg(weight, weight_units)
  height_cm = convert_length_to_m(height, height_units)*100
  bsa = calculate_bsa(height = height_cm, height_units = "cm", 
                      weight = weight_kg, weight_units = "kg",
                      method = "Gehan-George")
  
  ind0 = which(time==0)
  g0 = glucose[ind0]
  ins0 = insulin[ind0]

  ind90 = which(time==90)
  g90 = glucose[ind90]
  ins90 = insulin[ind90]

  ind120 = which(time==120)
  g120 = glucose[ind120]
  ins120 = insulin[ind120]
  
  p1=650
  p2=325
  p3=81300
  p4=132
  p5=652E-6
  p6=173
  V=10000  # L/m2 
  D=75/bsa # Dose in g/m2
  gcl=90   # equivalent Glucose Clamp Value
  
  x1 = p4*(((p1*D - V*(g120 - g90)/30)/g90 + p3/g0)/(ins90 - ins0 + p2))
  x2 = (p5*(g90-gcl) + 1)*x1
  ogis = 0.5*(x2 + sqrt((x2^2 + 4*p5*p6*(g90-gcl)*x1)))
  return(ogis)
  # return(c("x1"= x1, "x2"=x2, "ogis"=ogis, "bsa"=bsa, "Dose" = D))
}
