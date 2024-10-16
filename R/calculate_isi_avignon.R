#' Avignon Insulin Sensitivity Index Calculation (from standard 75g OGTT)
#'
#' This function calculates the Avignon Insulin Sensitivity Index using glucose
#' and insulin sampled during a standard 75g oral glucose tolerance test.
#' Methods described in \href{https://pubmed.ncbi.nlm.nih.gov/10375055/}{Avignon
#' et al.}
#'
#' Standard timepoints are 0, 30, 60, 90, and 120 min. Note: insulin unit
#' conversion may differ differ depending on assay. Insulin (pmol/l) = insulin
#' (uU/ml)*6
#'
#' `calculate_isi_avignon()` accepts 3 separate vectors for time, glucose,
#' insulin and a single weight value.
#' 
#' 
#' @inheritParams calculate_isi_matsuda
#' @param weight Weight (kg)
#' @param weight_units weight units, if not in kg
#'
#' @return A dataframe containing 3 values for basal, 2hr and mean Insulin
#'   Sensitivity values
#' @export
#' @examples
#' # individual objects for each item
#' time=c(0, 30, 60, 90, 120)              # minutes
#' glucose=c(93, 129, 178, 164, 97)        # mg/dL
#' insulin=c(12.8, 30.7, 68.5, 74.1, 44.0) # uU/mL
#' calculate_isi_avignon(time, glucose, insulin, weight=70)
#'
#' # handling data stored in a dataframe
#' ogtt1 <- data.frame(time=c(0, 30, 60, 90, 120),              # minutes
#'                     glucose=c(93, 129, 178, 164, 97),        # mg/dL
#'                     insulin=c(12.8, 30.7, 68.5, 74.1, 44.0)) # uU/mL
#' calculate_isi_avignon(ogtt1$time, ogtt1$glucose, ogtt1$insulin) #
#'
#' # example from Gutch et al 2015
#' ogtt2 <- data.frame(time=c(0, 30, 60, 90, 120),              # minutes
#'                     glucose=c(100, 160, 160, 160, 140),      # mg/dL
#'                     insulin=c(5, 10, 10, 10, 5))             # uU/mL
#' calculate_isi_avignon(ogtt2$time, ogtt2$glucose, ogtt2$insulin) #
#' calculate_isi_matsuda(ogtt2$time, ogtt2$glucose, ogtt2$insulin)
#'
#' # Convert units
#' ogtt5 <- data.frame(time = c(0,0.5,1,1.5,2), # time in hours
#'                     glucose = c(5.167, 7.167, 9.889, 9.111, 5.3889), # glucose in mmol/l
#'                     insulin = c(76.8,184.2,411,444.6,264)) # insulin in pmol/l
#'
#' calculate_isi_avignon(time =  ogtt5$time,
#'                         glucose = ogtt5$glucose,
#'                         insulin = ogtt5$insulin, weight=80,
#'                         time_units = "hr", insulin_units = "pmol/l", glucose_units = "mmol/l")

calculate_isi_avignon <- function(time, glucose, insulin, weight=NA,
                                  time_units = "min", glucose_units = "mg/dl", 
                                  insulin_units = "uU/ml", weight_units ="kg") {
  
  if (any(!is.numeric(time)) | any(!is.numeric(glucose)) | any(!is.numeric(insulin))) {
    rlang::warn("non-numeric values in time, glucose, or insulin")
    return(NA_real_)}
  
  # convert units; 
  time = convert_time_to_min(time, time_units)
  glucose = convert_glucose_to_mgdl(glucose, glucose_units)
  insulin = convert_insulin_to_uU_ml(insulin, insulin_units)
  weight = convert_weight_to_kg(weight, weight_units = weight_units)
  
  ind0 = which(time==0)
  g0 = glucose[ind0]
  ins0 = insulin[ind0]
  
  ind120 = which(time==120)
  g120 = glucose[ind120]
  ins120 = insulin[ind120]
  Vd = 150 # Vd glucose assumed to be 150 mL/kg BW
  
  avignon_basal = 1E8 / ((g0 * ins0) * weight * Vd) # Avignon Index at 0 min 
  avignon_2h = 1E8 / ((g120 * ins120) * weight * Vd) # Avignon Index at 120 min
  avignon_mean = (0.137*avignon_basal + avignon_2h)/2 # Avignon Index mean

  return(data.frame("isi_avignon_basal" = avignon_basal, 
              "isi_avignon_2h"= avignon_2h, 
              "isi_avignon_mean"= avignon_mean))
}

