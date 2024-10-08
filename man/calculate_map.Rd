% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_map.R
\name{calculate_map}
\alias{calculate_map}
\title{Calculate Mean Arterial Pressure}
\usage{
calculate_map(sbp, dbp)
}
\arguments{
\item{sbp}{Systolic blood pressure, mmHg}

\item{dbp}{Diastolic blood pressure, mmHg}
}
\value{
map as a single value, or vector of results
}
\description{
Returns the mean arterial pressure (MAP) from standard systolic (SBP) and diastolic (DBP) blood pressure measurements
}
\examples{
calculate_map(159, 46)
calculate_map("159", 46)
calculate_map("159", NA)
}
