#' Convert C-peptide concentration to uU/mL
#'
#' @param cpeptide numeric vector of cpeptide values in pmol/l
#' @param cpeptide_units character string of units (pmol/l or similar)
#'
#' @return numeric vector of cpeptide values in uU/mL
#' @name convert_cpeptide_to_
#'
#' @examples
#' convert_cpeptide_to_nM(1.0, cpeptide_units = "ng/ml")
#' convert_cpeptide_to_pM(1.0, cpeptide_units = "ng/ml")
#' convert_cpeptide_to_pM(1.0, cpeptide_units = "ng/ml")
#' convert_cpeptide_to_ng_ml(330, cpeptide_units = "pM")
#' convert_cpeptide_to_ng_ml(0.33, cpeptide_units = "nM")
#' convert_cpeptide_to_ng_ml(1, cpeptide_units = "ng/ml")

#' @rdname convert_cpeptide_to_
#' @export
convert_cpeptide_to_nM <- function(cpeptide, cpeptide_units){
  switch(cpeptide_units,
         "nmol/L" = cpeptide, "nmol/l" = cpeptide,"nmol_l" = cpeptide,
         "nM" = cpeptide,
         "ng/ml" = cpeptide*0.33, "ng/mL" = cpeptide*0.33,
         "ng_ml" = cpeptide*0.33,"ng_mL" = cpeptide*0.33,
         "pmol/L" = cpeptide/1000, "pM" = cpeptide/1000,
         stop("invalid units; use pmol/l or similar"))
}

#' @rdname convert_cpeptide_to_
#' @export
convert_cpeptide_to_pM <- function(cpeptide, cpeptide_units){
  switch(cpeptide_units,
         "pmol/L" = cpeptide, "pM" = cpeptide,
         "nmol/L" = cpeptide*1000, "nmol/l" = cpeptide*1000,"nmol_l" = cpeptide*1000,
         "nM" = cpeptide*1000,
         "ng/ml" = cpeptide*330, "ng/mL" = cpeptide*330,
         "ng_ml" = cpeptide*330,"ng_mL" = cpeptide*330,
         stop("invalid units; use pmol/l or similar"))
}

#' @rdname convert_cpeptide_to_
#' @export
convert_cpeptide_to_ng_ml <- function(cpeptide, cpeptide_units){
  switch(cpeptide_units,
         "ng/ml" = cpeptide, "ng/mL" = cpeptide,
         "ng_ml" = cpeptide,"ng_mL" = cpeptide,
         "pmol/L" = cpeptide/1000/0.33, "pM" = cpeptide/1000/0.33,
         "nmol/L" = cpeptide/0.33, "nmol/l" = cpeptide/0.33,"nmol_l" = cpeptide/0.33,
         "nM" = cpeptide/0.33,
         stop("invalid units; use pmol/l or similar"))
}

