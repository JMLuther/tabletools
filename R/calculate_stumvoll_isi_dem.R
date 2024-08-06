#' @export
#' @rdname calculate_stumvoll_isi

calculate_stumvoll_isi_dem <- function(time, glucose, insulin, bmi=NULL, age=NULL, 
                                   time_units = "min", 
                                    glucose_units = "mg/dl", insulin_units = "uU/ml") {
  
  if (any(!is.numeric(time)) | any(!is.numeric(insulin))) {
    rlang::warn("non-numeric values in time, glucose, or insulin")
    return(NA_real_)}
  
  if (!all(order(time) == seq_along(time))) {
    rlang::warn("Time values must be ordered from 0 to end") # check proper time order
    return(NA_real_)}
  
  # convert units; 
  time = convert_time_to_min(time, time_units)
  # glucose = convert_glucose_to_mM(glucose, glucose_units)
  insulin = convert_insulin_to_pM(insulin, insulin_units)
  
  # ind0 = which(time==0)
  # g0 = glucose[ind0]
  # ins0 = insulin[ind0]

  ind120 = which(time==120)
  g120 = glucose[ind120]
  ins120 = insulin[ind120]
  
  if (any(is.null(bmi)) | any(is.na(ins120)) | any(is.null(age))) {
    rlang::warn("Check for missing values in Age, BMI, or Insulin t=120")
    return(NA_real_)}
  
  stumvoll_isi =  0.222-0.00333*bmi - 0.0000779*ins120 - 0.000422*age
  
    return(stumvoll_isi)
}
