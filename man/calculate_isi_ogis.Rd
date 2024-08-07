% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_isi_ogis.R,
%   R/calculate_isi_ogis_180.R
\name{calculate_isi_ogis}
\alias{calculate_isi_ogis}
\alias{calculate_isi_ogis_180}
\title{OGTT Insulin Sensitivity Index Calculation (OGIS; from standard 75g OGTT)}
\usage{
calculate_isi_ogis(
  time,
  glucose,
  insulin,
  height,
  weight,
  time_units = "min",
  height_units = "m",
  weight_units = "kg",
  glucose_units = "mg/dl",
  insulin_units = "uU/ml"
)

calculate_isi_ogis_180(
  time,
  glucose,
  insulin,
  height,
  weight,
  time_units = "min",
  height_units = "m",
  weight_units = "kg",
  glucose_units = "mg/dl",
  insulin_units = "uU/ml"
)
}
\arguments{
\item{time}{a vector indicating time values (in minutes)}

\item{glucose}{a vecto storing glucose values (in mg/dL)}

\item{insulin}{a vector storing insulin values (in uU/mL)}

\item{height}{height, in meters (or )}

\item{weight}{weight, in kg}

\item{time_units}{if units are not in "min", can indicate here for unit
conversion (options "min" or "hr")}

\item{height_units}{if units are not in "m", can indicate here for unit
conversion (options "m" or "cm")}

\item{weight_units}{if units are not in "kg", can indicate here for unit
conversion (options "kg" or "lbs")}

\item{glucose_units}{if units are not in "mg/dl", can indicate here for unit
conversion (options "mg/dl" or "mmol/l")}

\item{insulin_units}{if units are not in "uU/ml", can indicate here for unit
conversion (options "uU/ml" or "pmol/l")}
}
\value{
OGIS Index as a single value (\eqn{ml\cdot min^{-1} \cdot m^2})
}
\description{
This function calculates the OGIS index using glucose and insulin sampled
during a standard 75g oral glucose tolerance test.
}
\details{
OGIS requires measurements at 0, 90, and 120 minutes. Alternatively the 3hr
version 'calculate_isi_ogis_180()' requires valuse at 0, 120, and 180 minutes.
can be used. Note: insulin unit conversion may differ differ depending on
assay. Insulin (pmol/l) = insulin (uU/ml)*6 Results are adjusted for BSA OGIS
is described in \href{https://pubmed.ncbi.nlm.nih.gov/11289482/}{Mari et
al.}. An Online calculator is
\href{http://webmet.pd.cnr.it/ogis/ogis.php}{available here.}

`calculate_isi_ogis()` accepts vectors for time, glucose, insulin, height, and
weight.
}
\examples{
# individual objects for each item
library(tabletools)
time = c(0,90,120)
glucose = c(90, 135, 115)
insulin = c(15, 150, 0)
calculate_isi_ogis(time = time,  # 344.5
               glucose = glucose,
               insulin = insulin,
               height=170, height_units = "cm",
               weight = 70, weight_units = "kg")
calculate_isi_ogis(time, glucose, insulin, height=1.70, weight = 70)
calculate_isi_ogis(time, glucose, insulin,
               height=70, height_units = "in",
               weight = 154, weight_units = "lbs")

time = c(0,1.5,2)
glucose = c(5.0, 7.5, 6.39)
insulin = c(90, 900, 0)
calculate_isi_ogis(time = time, time_units="hrs",  # 345
               glucose = glucose, glucose_units = "mmol/l",
               insulin = insulin, insulin_units = "pmol/l",
               height=170, height_units = "cm",
               weight = 70, weight_units = "kg")


time = c(0,120, 180)
glucose = c(90, 135, 115)
insulin = c(15, 150, 0)
calculate_isi_ogis_180(time = time,  # 344.5
               glucose = glucose,
               insulin = insulin,
               height=170, height_units = "cm",
               weight = 70, weight_units = "kg")
}
