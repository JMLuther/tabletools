% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_egfr_fas.R
\name{calculate_egfr_fas}
\alias{calculate_egfr_fas}
\title{Estimated GFR using Full Age Spectrum formula (FAS)}
\usage{
calculate_egfr_fas(age, sex, creatinine, creatinine_units = "mg/dL")
}
\arguments{
\item{age}{Age in years}

\item{sex}{Sex ("Male", "Female")}

\item{creatinine}{Serum Creatinine, in mg/dL}

\item{creatinine_units}{defaults to mg/dL}
}
\value{
a single value or vector of values for egfr
}
\description{
Uses age and sex-based median values for estimation of GFR as described by \href{https://pubmed.ncbi.nlm.nih.gov/26932693/}{Pottel et al.}
}
\examples{
# comparison vs 
calculate_egfr_fas(age=11, sex="Male", creatinine = 0.69)
calculate_egfr_fas(age=11.3, sex="feMale", creatinine = 0.63)

calculate_egfr_fas(age=53, sex="Male", creatinine = 1.1)
calculate_egfr_fas(age=53.5, sex="feMale", creatinine = 0.83)

calculate_egfr_fas(age=77.7, sex="Male", creatinine = 1.35)
calculate_egfr_fas(age=77.2, sex="feMale", creatinine = 0.98)
}
