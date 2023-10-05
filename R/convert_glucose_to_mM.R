#' Convert glucose from mg/dLl to mmol/L
#'
#' @param glucose numeric vector with glucose values
#' @param glucose_units currently only mg/dl 
#'
#' @return numeric vetor of glucose in mmol/l
#' @export convert_glucose_to_mM
#' @export convert_glucose_to_mgdl
#'
#' @examples
#' convert_glucose_to_mM(100, "mg/dl")
#' convert_glucose_to_mM(100, "mg/dL")
#' convert_glucose_to_mM(100, "mg_dl")
#' convert_glucose_to_mM(100, "mg") # improper units
#' convert_glucose_to_mM(100, "mmol/l") 
#' 
#' convert_glucose_to_mgdl(8, "mmol/l") 
#' 

convert_glucose_to_mM <- function(glucose, glucose_units){
  switch(glucose_units,
         "mg/dl" = glucose/18,
         "mg/dL" = glucose/18,
         mg_dl = glucose/18,
         "mmol/l" = glucose, "mmol/L" = glucose, mM = glucose, # no conversion
         stop("invalid units; use mg/dl or mmol/l"))
}

convert_glucose_to_mgdl <- function(glucose, glucose_units){
  switch(glucose_units,
         "mg/dl" = glucose,
         "mg/dL" = glucose,
         mg_dl = glucose,
         "mmol/l" = glucose*18, "mmol/L" = glucose*18, mM = glucose*18, # no conversion
         stop("invalid units; use mg/dl or mmol/l"))
}
