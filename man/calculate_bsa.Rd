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

\item{weight_units}{If `weight` is in Units other than kg, specifiy here to  properly convert for calucations ("kg", "g", or "lbs")}

\item{height_units}{If `height` is in Units other than m, specifiy here to  properly convert for calucations ("m", "cm", or "in")}

\item{method}{string for BSA formula to use for calculation ()}
}
\value{
a numeric vector with Body Surface Area (BSA, m^2)
}
\description{
Calculates Body Surface Area (m2) by several common methods. Converts weight to Kg and Height to cm if needed. Avaialbe methods for BSA calculations include:

  *  Mosteller: SQRT(Ht(cm) x Wt(kg) / 3600   
  *  DuBois and DuBois: Wt(kg)^(0.425) x Ht(cm)^(0.725) x 0.007184  
  *  Gehan and George: Wt(kg)^(0.51456) x Ht(cm)^(0.42246) x 0.0235  
  *  Haycock: Wt(kg)^(0.5378) x Ht(cm)^(0.3964) x 0.024265  
  *  Yu: 71.3989 x Ht(cm)^(0.7437) x Wt(kg)^(0.4040) / 10000  
  *  Livingston: 0.1173 x Wt(kg)^(0.6466)   
  *  Tikuisis: 128.1 x Ht(cm)^(0.60) x Wt(kg)^(0.44)
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