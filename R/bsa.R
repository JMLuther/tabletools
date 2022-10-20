#' Body Surface Area Calculation
#' 
#' @description Calculates Body Surface Area (m2) by several common methods. Converts weight to Kg and Height to cm if needed. 
#' 
#' Avaialbe methods for BSA calculations include:

#' \begin{equation}
#' \sqrt{\frac{Ht(cm) \cdot Wt(kg)}{3600}} \tag{Mosteller}
#'  \end{equation}
#' 
#' \begin{equation}
#'  Wt(kg)^{0.425} \cdot Ht(cm)^{0.725} \cdot 0.007184 \tag{DuBois and DuBois}
#'  \end{equation}
#' 
#'  \begin{equation}
#'  Wt(kg)^{0.51456} \cdot Ht(cm)^{0.42246} \cdot 0.0235 \tag{Gehan and George}
#'  \end{equation}
#' 
#'  \begin{equation}
#'  Wt(kg)^{0.5378} \cdot Ht(cm)^{0.3964} \cdot 0.024265 \tag{Haycock}
#'  \end{equation}
#' 
#' \begin{equation}
#' 71.3989 \cdot Ht(cm)^{0.7437} \cdot Wt(kg)^{0.4040} \div 10000 \tag{Yu}
#' \end{equation}
#' 
#' \begin{equation}
#' 0.1173 \cdot Wt(kg)^{0.6466} \tag{Livingston}
#' \end{equation}
#' 
#' \begin{equation}
#' 128.1 \cdot Ht(cm)^{0.60} \cdot Wt(kg)^{0.44} \tag{Tikuisis}
#' \end{equation}
#' 
#' 
#' @param weight Body Weight (kg)
#' @param height Body Height (m)
#' @param weight_units If `weight` is in Units other than kg, specifiy here to  properly convert for calucations ("kg", "g", or "lbs")
#' @param height_units If `height` is in Units other than m, specifiy here to  properly convert for calucations ("m", "cm", or "in")
#' @param method Which BSA formula to use for calculation ()

#' @returns a numeric vector
#' @export
#' @examples 
#' bsa_methods <- c("Mosteller", "DuBois", "Boyd", "Gehan-George", "Haycock", "Yu", "Livingston", "Tikuisis")
#' names(bsa_methods) <- bsa_methods
#' sapply(bsa_methods, \(x)bsa(weight = 70, height = 1.778, method = x))
#' bsa(weight = 70, height = 1.778)
#' bsa(weight = 70, height = 1.778, method = "Mosteller")
#' bsa(weight = 70, height = 1.778, method = "DuBois")
#' bsa(weight = 70, height = 1.778, method = "Boyd")
#' bsa(weight = 70, height = 1.778, method = "Gehan-George")
#' bsa(weight = 70, height = 1.778, method = "Haycock")
#' bsa(weight = 70, height = 1.778, method = "Yu")
#' bsa(weight = 70, height = 1.778, method = "Livingston")
#' bsa(weight = 70, height = 1.778, method = "Tikuisis")
#' 
#' # different units
#' bsa(weight = 154, weight_units = "lbs", 
#'     height = 70, height_units = "in", 
#'     method = "Mosteller")
#' bsa(weight = 70, height = 1.778)

bsa <- function(weight, height, weight_units = "kg", height_units = "m", method = "Mosteller", ...) {
  # convert weight units to kg
  if (weight_units == "kg") {
    Wt = weight
  } else if (weight_units == "g") {
    Wt = weight *1000
  } else if (weight_units == "lbs") {
    Wt = weight / 2.2}

  # convert height units to cm
  if (height_units == "m") {
    Ht = height * 100
  } else if (height_units == "cm") {
    Ht = height
  }  else if (height_units == "in") {
    Ht = height*2.54}

  # choose proper method 
  if (method == "Mosteller") {
    return(sqrt(Ht*Wt/3600))
  } else if (method == "DuBois"){
    return(Wt^0.425*Ht^0.725*0.007184)
  } else if (method == "Boyd"){
    return(0.0003207*Ht^(0.3)*(Wt*1000)^(0.7285-(0.0188*log((Wt*1000), 10))))
  } else if (method == "Gehan-George"){
    return(Wt^0.51456 * Ht^0.42246 * 0.0235)
  } else if (method == "Haycock"){
    return(Wt^0.5378 * Ht^0.3964 * 0.024265)
  } else if (method == "Yu"){
    return(71.3989 * Ht^0.7437 * Wt^0.4040 / 10000) 
  } else if (method == "Livingston"){
    return(0.1173 * Wt^0.6466)
  } else if (method == "Tikuisis"){
    return(128.1 * Ht^0.60 * Wt^0.44 / 10000)
  }
  
}
