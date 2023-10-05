#' Convert Insulin concentration to uU/mL
#'
#' @param insulin numeric vector of insulin values in pmol/l
#' @param insulin_units character string of units (pmol/l or similar)
#'
#' @return numeric vector of insulin values in uU/mL
#' @export convert_insulin_to_uU_ml
#' @export convert_insulin_to_pM
#'
#' @examples
#' convert_insulin_to_uU_ml(50, "pmol/L")
#' convert_insulin_to_pM(8.3333, "uu/ml")

convert_insulin_to_uU_ml <- function(insulin, insulin_units){
  switch(insulin_units,
         "uU/ml" = insulin,"uU/mL" = insulin, "uu/ml" = insulin,
         uU_ml = insulin, uu_ml = insulin, uUml = insulin, 
         "pmol/l" = insulin/6,
         "pmol/L" = insulin/6,
         "pM" = insulin/6,
         stop("invalid units; use pmol/l or similar"))
}

convert_insulin_to_pM <- function(insulin, insulin_units){
  switch(insulin_units,
         "pmol/l" = insulin,
         "pmol/L" = insulin,
         "pM" = insulin,
         "uU/ml" = insulin*6,"uU/mL" = insulin*6, "uu/ml" = insulin*6,
         uU_ml = insulin*6, uu_ml = insulin*6, uUml = insulin*6, 
         stop("invalid units; use pmol/l or similar"))
}
