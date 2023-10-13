#' Glucose Tolerance Test Classification
#'
#' Factor the 2-hour glucose from a standard 75g OGTT
#'
#' * `factor_glucose_tolerance()` classifies glucose tolerance from a 2-hr OGTT
#' glucose as Normal (NGT), Impaired (IGT), or Diabetes (DM2)
#'
#' @param x A glucose value in mg/dL. Can be single value or a vector. Converts
#'   a number entered as a string to numeric if possible, otherwise returns `NA`
#'
#' @return A Factor for OGTT classification of fasting and 2hr glucose
#' @export factor_glucose_tolerance
#' 
#'
#' @examples
#' factor_glucose_tolerance(140)
#' factor_glucose_tolerance(139.9)
#' factor_glucose_tolerance(199.9)
#' factor_glucose_tolerance(200)
#' factor_glucose_tolerance("0")


factor_glucose_tolerance <- function(x) {
  cut(as.numeric(x), # returns NA if not numeric
      breaks = c(0, 140, 200, Inf), 
      labels = c("NGT", "IGT", "DM2"),
      right = F)
}
