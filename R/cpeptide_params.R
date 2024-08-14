#' C-peptide parameters estimates from population data
#'
#' This function estimates C-peptide kinetic parameters from population data.
#' Based on the regression models from
#' \href{https://pubmed.ncbi.nlm.nih.gov/1551497/}{Van Cauter et al}.
#' Participants were considered "Obese" in this study if body weight was >15%
#' above ideal body weight, rather than a commonly used BMI cutoff. 
#'
#' @param age Age in years
#' @param gender Gender
#' @param height Height in meters
#' @param weight Weight in kg
#' @param category Category ("Normal", "Obese", "NIDDM")
#' @param weight_units weight units, if not in kg
#' @param height_units height units, if not in meters
#'
#' @return Data frame with Vd, halflife_short, halflife_long, fraction, bsa,
#'   k12, k21, k10
#' @export
#'
#' @examples
#' cpeptide_params(age=28.1, height = 1.744666, weight = 69.4, gender = "F", category = "normal")
#' cpeptide_params(age=28.1, height = 1.744666, weight = 69.4, gender = "M", category = "normal")
#' cpeptide_params(age=35.2, height = 1.678461, weight = 107.9, gender = "F", category = "obese")
#' cpeptide_params(age=35.2, height = 1.678461, weight = 107.9, gender = "M", category = "obese")

cpeptide_params <- function(age, gender = NA, height, weight, 
                            category = "normal",
                            weight_units = "kg", height_units = "m"){
  
  # convert units, calculation bsa
  weight_kg = convert_weight_to_kg(weight, weight_units = weight_units)
  height_m = convert_height_to_m(height, height_units = height_units)
  bsa = calculate_bsa(weight=weight_kg, height = height_m)
  # Age in years
  # category = c("normal", "obese", "NIDDM")
  halflife_short = # minutes
    ifelse(category %in% c("normal", "Normal"), 4.95,
           ifelse(category %in% c("obese", "Obese"), 4.55,
                  ifelse(category %in% c("niddm", "NIDDM", "T2DM", "DM", "Diabetic", "Diabetes"), 4.52, NA)))
  fraction =
    ifelse(category %in% c("normal", "Normal"), 0.76,
           ifelse(category %in% c("obese", "Obese"), 0.78,
                  ifelse(category %in% c("niddm", "NIDDM", "T2DM", "DM", "Diabetic", "Diabetes"), 0.78, NA)))
  halflife_long = 0.14*age+29.16 # minutes
  
  Vd = ifelse(gender %in% c("Female", "female", "F", "Fe"), 1.11*bsa+2.04,
              ifelse(gender %in% c("Male", "male", "M", "Ma"), 1.92*bsa+0.64, NA))
  a=log(2)/halflife_short
  b=log(2)/halflife_long
  k21= fraction*b + (1-fraction)*a # k1
  k10= a*b/k21                     # k2
  k12= a+b-k10-k21                 # k3
  
  return (data.frame(Vd, halflife_short, halflife_long, fraction, bsa, k12, k21, k10))
}