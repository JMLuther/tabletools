% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_homair.R
\name{calculate_homair}
\alias{calculate_homair}
\title{Calculate HOMA-IR from fasting glucose and insulin}
\usage{
calculate_homair(
  glucose,
  insulin,
  glucose_units = "mg/dl",
  insulin_units = "uU/ml"
)
}
\arguments{
\item{glucose}{fasting glucose (mg/dL)}

\item{insulin}{fasting insulin (uU/mL)}

\item{glucose_units}{if units are not in "mg/dl", can indicate here for unit conversion (options "mg/dl" or "mmol/l")}

\item{insulin_units}{if units are not in "uU/ml", can indicate here for unit conversion (options "uU/ml" or "pmol/l")}
}
\value{
HOMA-IR estimate of insulin sensitivity, as a single numeric value
}
\description{
Uses the simple formula \eqn{Glucose(mM) \cdot Insulin(uU/mL) /22.5}
}
\examples{
calculate_homair(glucose = c(93), insulin = c(12.8))
calculate_homair(93, 12.8)
calculate_homair(100, 5) # 1.23; ex from Gutch 2015
calculate_homair(93, NA) # error
calculate_homair(5.167, 76.8, glucose_units = "mmol/l", insulin_units = "pmol/l")
}
