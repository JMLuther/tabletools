#' Glucose Tolerance Test Classification
#'
#' Functions to factor the fasting and 2-hour glucose from a standard 75g OGTT
#'
#' * `factor_glucosetolerance_0()` classifies fasting glucose as Normal (NFG),
#' Impaired (IFG), or Diabetes (DM2)
#' * `factor_glucosetolerance_120()` classifies glucose tolerance from a 2-hr OGTT
#' glucose as Normal (NGT), Impaired (IGT), or Diabetes (DM2)
#'
#' @param x A glucose value in mg/dL. Can be single value or a vector. Converts
#'   a number entered as a string to numeric if possible, otherwise returns `NA`
#'
#' @return A Factor for OGTT classification of fasting and 2hr glucose
#' @export factor_glucosetolerance_0
#' @export factor_glucosetolerance_120
#' 
#'
#' @examples
#' factor_glucosetolerance_120(140)
#' factor_glucosetolerance_120(139.9)
#' factor_glucosetolerance_120(199.9)
#' factor_glucosetolerance_120(200)
#' factor_glucosetolerance_120("0")
#' factor_glucosetolerance_0(c(120, 130, 150, 200))

factor_glucosetolerance_0 <- function(x) {
  cut(as.numeric(x),  # returns NA if not numeric
      breaks = c(0, 100, 125, Inf),
      labels = c("NFG", "IFG", "DM2"),
      right = F)
}


factor_glucosetolerance_120 <- function(x) {
  cut(as.numeric(x), # returns NA if not numeric
      breaks = c(0, 140, 200, Inf), 
      labels = c("NGT", "IGT", "DM2"),
      right = F)
}
