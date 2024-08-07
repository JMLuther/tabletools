% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_bsa.R
\name{calculate_bsa}
\alias{calculate_bsa}
\title{Body Surface Area Calculation}
\usage{
calculate_bsa(
  weight,
  height,
  weight_units = "kg",
  height_units = "m",
  method = "Mosteller"
)
}
\arguments{
\item{weight}{Body Weight (kg)}

\item{height}{Body Height (m)}

\item{weight_units}{If \code{weight} is in Units other than kg, specifiy here to  properly convert for calucations ("kg", "g", or "lbs")}

\item{height_units}{If \code{height} is in Units other than m, specifiy here to  properly convert for calucations ("m", "cm", or "in")}

\item{method}{string for BSA formula to use for calculation ()}
}
\value{
a numeric vector with Body Surface Area (BSA, m^2)
}
\description{
Calculates Body Surface Area (m2) by several common methods. Converts weight to Kg and Height to cm if needed.
Available methods for BSA calculations are described in \href{https://pubmed.ncbi.nlm.nih.gov/19900761/}{Yu et al.}:
\itemize{
\item Mosteller: \eqn{\frac{\sqrt{Ht(cm) \cdot Wt(kg)} }{3600}}
\item DuBois and DuBois: \eqn{Wt(kg)^{0.425} \cdot Ht(cm)^{0.725} \cdot 0.007184}
\item Gehan and George: \eqn{Wt(kg)^{0.51456} \cdot Ht(cm)^{0.42246} \cdot 0.0235}
\item Haycock: \eqn{Wt(kg)^{0.5378} \cdot Ht(cm)^{0.3964} \cdot 0.024265 }
\item Yu: \eqn{\frac{71.3989 \cdot Ht(cm)^{0.7437} \cdot Wt(kg)^{0.4040}}{10000} }
\item Livingston: \eqn{0.1173 \cdot Wt(kg)^{0.6466}  }
\item Tikuisis: \eqn{128.1 \cdot Ht(cm)^{0.60} \cdot Wt(kg)^{0.44} }
}
}
\examples{
bsa_methods <- c("Mosteller", "DuBois", "Boyd", "Gehan-George", "Haycock", "Yu", "Livingston", "Tikuisis")
names(bsa_methods) <- bsa_methods
sapply(bsa_methods, \(x)calculate_bsa(weight = 70, height = 1.778, method = x))
calculate_bsa(weight = 70, height = 1.778)
calculate_bsa(weight = 70, height = 1.778, method = "Mosteller")
calculate_bsa(weight = 70, height = 1.778, method = "DuBois")
calculate_bsa(weight = 70, height = 1.778, method = "Boyd")
calculate_bsa(weight = 70, height = 1.778, method = "Gehan-George")
calculate_bsa(weight = 70, height = 1.778, method = "Haycock")
calculate_bsa(weight = 70, height = 1.778, method = "Yu")
calculate_bsa(weight = 70, height = 1.778, method = "Livingston")
calculate_bsa(weight = 70, height = 1.778, method = "Tikuisis")

# different units
calculate_bsa(weight = 154, weight_units = "lbs", 
    height = 70, height_units = "in", 
    method = "Mosteller")
calculate_bsa(weight = 70, height = 1.778)
}
