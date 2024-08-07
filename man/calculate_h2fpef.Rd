% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_h2fpef.R
\name{calculate_h2fpef}
\alias{calculate_h2fpef}
\title{H2PEF calculator
Returns the H2FPEF}
\usage{
calculate_h2fpef(age, bmi, pa_pressure, filling_pressure, afib)
}
\arguments{
\item{age}{Age (years)}

\item{bmi}{BMI, kg/m^2}

\item{pa_pressure}{estimated Pulmonary Artery Pressure (mm/hg)}

\item{filling_pressure}{(Doppler echo E/e')}

\item{afib}{Atrial Fibrillation (T/F)}
}
\value{
H2PEF probability (%)
}
\description{
Estimated probability of HFpEF based on a validated risk calculator \href{https://pubmed.ncbi.nlm.nih.gov/29792299/}{(Reddy 2018)}.
This calculator uses the continuous model provided online, downloadable as an XLS file,
rather than the point-based estimate. Results of this function have been checked against this online calculator.
}
\examples{
calculate_h2fpef(age=45, bmi=31, filling_pressure=9, pa_pressure = 35, afib = F)
calculate_h2fpef(age=60, bmi=25, filling_pressure=5, pa_pressure = 12, afib = T)
calculate_h2fpef(age=51, bmi=30, filling_pressure=10, pa_pressure = 45, afib = T)
}
