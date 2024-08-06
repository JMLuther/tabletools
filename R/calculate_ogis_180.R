#' @rdname calculate_ogis 
#' @export

calculate_ogis_180 <- function(time, glucose, insulin, height, weight, 
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
  
  # ind90 = which(time==90)
  # g90 = glucose[ind90]
  # ins90 = insulin[ind90]
  
  ind120 = which(time==120)
  g120 = glucose[ind120]
  ins120 = insulin[ind120]
  
  ind180 = which(time==180)
  g180 = glucose[ind180]
  ins180 = insulin[ind180]
  
  p1=289
  p2=270
  p3=14000
  p4=440
  p5=637E-6
  p6=117
  V=10000  # L/m2 
  D=75/bsa # Dose in g/m2
  gcl=90   # equivalent Glucose Clamp Value
  
  x1 = p4*(((p1*D - V*(g180 - g120)/60)/g120 + p3/g0)/(ins120 - ins0 + p2))
  x2 = (p5*(g120-gcl) + 1)*x1
  ogis = 0.5*(x2 + sqrt((x2^2 + 4*p5*p6*(g120-gcl)*x1)))
  return(ogis)
  # return(c("x1"= x1, "x2"=x2, "ogis"=ogis, "bsa"=bsa, "Dose" = D))
}
