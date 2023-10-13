#' Fasting Glucose Classification
#'
#' * `factor_glucose_fasting()` classifies fasting glucose as Normal (NFG),
#' Impaired (IFG), or Diabetes (DM2)
#'
#' @param x A glucose value, or vector of values, in mg/dL. Converts
#'   a number entered as a string to numeric if possible, otherwise returns `NA`
#'
#' @return A Factor for OGTT classification of fasting and 2hr glucose
#' @export factor_glucose_fasting
#' 
#'
#' @examples
#' factor_glucose_fasting(155)
#' factor_glucose_fasting(c(120, 130, 150, 200))
#' factor_glucose_fasting(c(120, 130, NA, 200))

factor_glucose_fasting <- function(x) {
  cut(as.numeric(x),  # returns NA if not numeric
      breaks = c(0, 100, 125, Inf),
      labels = c("NFG", "IFG", "DM2"),
      right = F)
}
