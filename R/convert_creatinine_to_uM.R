#' Convert Cretainine from mg/dL to umol/L
#'
#' @param creatinine numeric vector with creatinine values
#' @param creatinine_units currently only mg/dl 
#'
#' @return numeric vetor of creatinine in umol/l
#' @export convert_creatinine_to_uM
#' @export convert_creatinine_to_mgdl
#'
#' @examples
#' convert_creatinine_to_uM(1, "mg/dl")
#' convert_creatinine_to_uM(1, "mg/dL")
#' convert_creatinine_to_uM(1, "mg_dl")
#' convert_creatinine_to_uM(1, "mg") # improper units
#' convert_creatinine_to_uM(1, "umol/l") 
#' 
#' convert_creatinine_to_mgdl(88.4, "umol/l") 
#' 

convert_creatinine_to_uM <- function(creatinine, creatinine_units="mg/dL"){
  switch(creatinine_units,
         "mg/dl" = creatinine*88.4,
         "mg/dL" = creatinine*88.4,
         mg_dl = creatinine*88.4,
         "umol/l" = creatinine, "umol/L" = creatinine, uM = creatinine, # no conversion
         stop("invalid units; use mg/dl or umol/l"))
}

convert_creatinine_to_mgdl <- function(creatinine, creatinine_units){
  switch(creatinine_units,
         "mg/dl" = creatinine,
         "mg/dL" = creatinine,
         mg_dl = creatinine,
         "umol/l" = creatinine/88.4, "umol/L" = creatinine/88.4, uM = creatinine/88.4, # no conversion
         stop("invalid units; use mg/dl or umol/l"))
}
