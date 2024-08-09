#' Convert Cholesterol (Total, HDL, or LDL) from mg/dL to mmol/L
#'
#' LDL, HDL and Total cholesterol are usually reported in mg/dL units. Because
#' the value is the cholesterol content, all use the same unit conversion.
#' 
#' @param cholesterol numeric vector with cholesterol values
#' @param cholesterol_units currently only mg/dl
#'
#' @return numeric vetor of cholesterol in mmol/l
#' @export convert_cholesterol_to_mM
#' @export convert_cholesterol_to_mgdl
#'
#' @examples
#' convert_cholesterol_to_mM(200, "mg/dl")
#' convert_cholesterol_to_mM(100, "mg/dL")
#' convert_cholesterol_to_mM(100, "mg_dl")
#' convert_cholesterol_to_mM(100, "mg") # improper units
#' convert_cholesterol_to_mM(100, "mmol/l")
#' convert_cholesterol_to_mgdl(5.171968, "mmol/l")

convert_cholesterol_to_mM <- function(cholesterol, cholesterol_units="mg/dL"){
  switch(cholesterol_units,
         "mg/dl" = cholesterol/38.67,
         "mg/dL" = cholesterol/38.67,
         mg_dl = cholesterol/38.67,
         "mmol/l" = cholesterol, "mmol/L" = cholesterol, mM = cholesterol, # no conversion
         stop("invalid units; use mg/dl or mmol/l"))
}

convert_cholesterol_to_mgdl <- function(cholesterol, cholesterol_units){
  switch(cholesterol_units,
         "mg/dl" = cholesterol,
         "mg/dL" = cholesterol,
         mg_dl = cholesterol,
         "mmol/l" = cholesterol*38.67, "mmol/L" = cholesterol*38.67, mM = cholesterol*38.67, # no conversion
         stop("invalid units; use mg/dl or mmol/l"))
}
