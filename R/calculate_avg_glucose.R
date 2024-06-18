#' Estimate average glucose from A1C value
#'
#' @param a1c measured A1C
#' @param glucose_units Desired glucose units for result- default is "mg/dl",
#'   can indicate here for unit conversion options, eg "mg/dl" or "mmol/l"
#'
#' @return estimated average glucose with mg/dl default, as a single numeric value
#' @export
#' @details Conversion from A1C to estimated average glucose using linear
#' interpolation described in the paper
#' \href{https://pubmed.ncbi.nlm.nih.gov/18540046/}{"Translating the A1C assay
#' into estimated average glucose values"}
#'
#'
#' @examples
#' calculate_avg_glucose(5)
#' calculate_avg_glucose(5:12)
#' calculate_avg_glucose(5:12, glucose_units = "mM")


calculate_avg_glucose <- function(a1c,  glucose_units = "mg/dl") {
  if (any(!is.numeric(a1c))) {
    return(NA_real_)
    warning("non-numeric values in A1C")} 

  # convert units to glucose mg/dl 
  glucose = 28.7 * a1c - 46.7 # in mg/dl
  switch(glucose_units,
         "mg/dl" = glucose,
         "mg/dL" = glucose,
         mg_dl = glucose,
         "mmol/l" = glucose/18, 
         "mmol/L" = glucose/18, 
         mM = glucose/18, 
         stop("invalid units; use mg/dl or mmol/l"))
}
