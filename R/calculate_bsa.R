#' Body Surface Area Calculation
#' 
#' @description Calculates Body Surface Area (m2) by several common methods. Converts weight to Kg and Height to cm if needed. 
#' Available methods for BSA calculations are described in \href{https://pubmed.ncbi.nlm.nih.gov/19900761/}{Yu et al.}:
#' 
#'  *  Mosteller: \eqn{\frac{\sqrt{Ht(cm) \cdot Wt(kg)} }{3600}}
#'  *  DuBois and DuBois: $Wt(kg)^{0.425} \cdot Ht(cm)^{0.725} \cdot 0.007184 $
#'  *  Gehan and George: $Wt(kg)^{0.51456} \cdot Ht(cm)^{0.42246} \cdot 0.0235  $
#'  *  Haycock: $Wt(kg)^{0.5378} \cdot Ht(cm)^{0.3964} \cdot 0.024265  $
#'  *  Yu: $\frac{71.3989 \cdot Ht(cm)^{0.7437} \cdot Wt(kg)^{0.4040}}{10000}  $
#'  *  Livingston: $0.1173 \cdot Wt(kg)^{0.6466}   $
#'  *  Tikuisis: $128.1 \cdot Ht(cm)^{0.60} \cdot Wt(kg)^{0.44}  $
#' 
#' @param weight Body Weight (kg)
#' @param height Body Height (m)
#' @param weight_units If `weight` is in Units other than kg, specifiy here to  properly convert for calucations ("kg", "g", or "lbs")
#' @param height_units If `height` is in Units other than m, specifiy here to  properly convert for calucations ("m", "cm", or "in")
#' @param method string for BSA formula to use for calculation ()
#'
#' @returns a numeric vector with Body Surface Area (BSA, m^2)
#' @export
#' @md
#' 
#' @examples 
#' bsa_methods <- c("Mosteller", "DuBois", "Boyd", "Gehan-George", "Haycock", "Yu", "Livingston", "Tikuisis")
#' names(bsa_methods) <- bsa_methods
#' sapply(bsa_methods, \(x)calculate_bsa(weight = 70, height = 1.778, method = x))
#' calculate_bsa(weight = 70, height = 1.778)
#' calculate_bsa(weight = 70, height = 1.778, method = "Mosteller")
#' calculate_bsa(weight = 70, height = 1.778, method = "DuBois")
#' calculate_bsa(weight = 70, height = 1.778, method = "Boyd")
#' calculate_bsa(weight = 70, height = 1.778, method = "Gehan-George")
#' calculate_bsa(weight = 70, height = 1.778, method = "Haycock")
#' calculate_bsa(weight = 70, height = 1.778, method = "Yu")
#' calculate_bsa(weight = 70, height = 1.778, method = "Livingston")
#' calculate_bsa(weight = 70, height = 1.778, method = "Tikuisis")
#' 
#' # different units
#' calculate_bsa(weight = 154, weight_units = "lbs", 
#'     height = 70, height_units = "in", 
#'     method = "Mosteller")
#' calculate_bsa(weight = 70, height = 1.778)

calculate_bsa <- function(weight, height, weight_units = "kg", height_units = "m", method = "Mosteller") {

  if (any(is.na(weight)) | any(is.na(height))) {
    rlang::warn("Check for missing values in height, weight")
    return(NA_real_)}
  
  if (any(!is.numeric(weight)) | any(!is.numeric(height))) {
    rlang::warn("non-numeric values in height, weight")
    return(NA_real_)}
  
  # convert height and weight units to cm, kg
  Wt = convert_weight_to_kg(weight, weight_units)
  Ht = convert_height_to_m(height, height_units)*100

  # choose proper method 
  switch(method,
         Mosteller = sqrt(Ht*Wt/3600),
         DuBois = Wt^0.425*Ht^0.725*0.007184,
         Boyd = 0.0003207*Ht^(0.3)*(Wt*1000)^(0.7285-(0.0188*log((Wt*1000), 10))),
         "Gehan-George" = Wt^0.51456 * Ht^0.42246 * 0.0235,
         Haycock = Wt^0.5378 * Ht^0.3964 * 0.024265,
         Yu = 71.3989 * Ht^0.7437 * Wt^0.4040 / 10000, 
         Livingston = 0.1173 * Wt^0.6466,
         Tikuisis = 128.1 * Ht^0.60 * Wt^0.44 / 10000,
         stop("invalid method; please use 'method = ' one of the following: 
              Mosteller, DuBois, Boyd, Gehan-George, Haycock, Yu, Livingston, Tikuisis" ))
  }

