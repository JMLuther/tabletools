#' Convert Triglycerides from mg/dL to mmol/L
#'
#' LDL, HDL and Total triglycerides are usually reported in mg/dL units. 
#' 
#' @param triglycerides numeric vector with triglycerides values
#' @param triglycerides_units currently only mg/dl
#'
#' @return numeric vetor of triglycerides in mmol/l
#' @export convert_triglycerides_to_mM
#' @export convert_triglycerides_to_mgdl
#'
#' @examples
#' convert_triglycerides_to_mM(200, "mg/dl")
#' convert_triglycerides_to_mM(150, "mg/dL")
#' convert_triglycerides_to_mM(100, "mg_dl")
#' convert_triglycerides_to_mM(100, "mg") # improper units
#' convert_triglycerides_to_mM(100, "mmol/l")
#' convert_triglycerides_to_mgdl(1.693576, "mmol/l")

convert_triglycerides_to_mM <- function(triglycerides, triglycerides_units="mg/dL"){
  switch(triglycerides_units,
         "mg/dl" = triglycerides/88.57,
         "mg/dL" = triglycerides/88.57,
         mg_dl = triglycerides/88.57,
         "mmol/l" = triglycerides, "mmol/L" = triglycerides, mM = triglycerides, # no conversion
         stop("invalid units; use mg/dl or mmol/l"))
}

convert_triglycerides_to_mgdl <- function(triglycerides, triglycerides_units){
  switch(triglycerides_units,
         "mg/dl" = triglycerides,
         "mg/dL" = triglycerides,
         mg_dl = triglycerides,
         "mmol/l" = triglycerides*88.57, "mmol/L" = triglycerides*88.57, mM = triglycerides*88.57, # no conversion
         stop("invalid units; use mg/dl or mmol/l"))
}
