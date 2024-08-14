#' C-peptide parameters estimates from population data
#'
#' This function estimates C-peptide kinetic parameters from population data.
#' Based on the regression models from
#' \href{https://pubmed.ncbi.nlm.nih.gov/1551497/}{Van Cauter et al}.
#' Participants were considered "Obese" in this study if body weight was >15%
#' above ideal body weight, rather than a commonly used BMI cutoff.
#'
#' The parameters can be used in calculations to estimate insulin secretion
#' rates, shown in the example.
#'
#' @param age Age in years
#' @param gender Gender
#' @param height Height in meters
#' @param weight Weight in kg
#' @param category Category ("Normal", "Obese", "NIDDM")
#' @param weight_units weight units, if not in kg
#' @param height_units height units, if not in meters
#'
#' @return Data frame with variables `cp_vd`, `cp_halflife_short`, `cp_halflife_long`,
#'   `cp_fraction`, `cp_bsa`, `cp_k12`, `cp_k21`, `cp_k10`
#' @export
#'
#' @examples
#' cpeptide_params(age=28.1, height = 1.744666, weight = 69.4, gender = "F", category = "normal")
#' cpeptide_params(age=28.1, height = 1.744666, weight = 69.4, gender = "M", category = "normal")
#' cpeptide_params(age=35.2, height = 1.678461, weight = 107.9, gender = "F", category = "obese")
#' cpeptide_params(age=35.2, height = 1.678461, weight = 107.9, gender = "M", category = "obese")
#' 
#' library(dplyr)
#' dat <- data.frame(id =1:10,
#'   age = rnorm(10, 50, sd=8),
#'   gender = sample(c("M", "F"), 10, replace = T),
#'   height = rnorm(10, 1.7, sd=0.2),
#'   weight = rnorm(10, 100, sd=20),
#'   cpeptide_0 =  abs(rnorm(10, 2.0, 0.91))  ) # ng/dL
#'
#' dat |>
#'   mutate(ibw = calculate_ibw(height, gender, height_units = "m", weight_units = "kg"),
#'          ibw_pct = (weight/ibw-1)*100,
#'          ibw_15 = case_when(ibw_pct>15 ~ "obese", # if >15% above IBW then Obese
#'                             TRUE ~ "normal"),
#'          cpeptide_0_pmol = convert_cpeptide_to_pM(cpeptide_0, cpeptide_units="ng/ml"),
#'          cpeptide_params(age, gender, height, weight, category = ibw_15),
#'          # Basal Insulin Secretion Rate (pmol/min)
#'          ISR_0 = cp_k10*cpeptide_0_pmol*cp_vd)


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
  
  return (data.frame("cp_vd" = Vd, 
                     "cp_halflife_short"=halflife_short, 
                     "cp_halflife_long"=halflife_long, 
                     "cp_fraction"=fraction, 
                     "cp_bsa"=bsa, 
                     "cp_k12"=k12, "cp_k21"=k21, "cp_k10"=k10))
}
