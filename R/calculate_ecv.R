#' Estimate Extracellular Volume
#'
#' Calculates estimated extracellular volume based on available population-based
#' regression equations (age, height, weight, sex). Formula described in
#' \href{https://pubmed.ncbi.nlm.nih.gov/35497800/}{Faucon et al. KI Rep. 2022}.
#' Comparison of different formulas suggests that the new formula provides more
#' accurate results. The "Granerus" equations have been used previously in
#' calculating single-sample iothalamate GFR and so this method is also
#' available.
#'
#' @param age Age in years
#' @param height Height (cm)
#' @param weight Weight (kg)
#' @param sex Sex (M/F)
#' @param weight_units Weight units if not in cm
#' @param height_units Height units if not in kg
#' @param method ECV Formula (default =  "Faucon"; also can use "Granerus")
#'
#' @return single value of ECV estimate in mL
#' @export
#'
#' @examples
#' calculate_ecv(age=85, sex = "m", height = 187, weight=85)
#' calculate_ecv(age=85, sex = "m", height = 187, weight=85, method="Granerus")
#' calculate_ecv(age=85, sex = "f", height = 187, weight=85, method="Granerus")

calculate_ecv <- function(age, height, weight, sex, 
                          weight_units="kg",
                          height_units="cm",
                          method="Faucon"){
  weight_kg = convert_weight_to_kg(weight=weight, weight_units = weight_units)
  height_cm = convert_length_to_m(height, length_units = height_units)*100
  alpha = ifelse(tolower(sex) %in% c("male", "m"), -2.6631,
                 ifelse(tolower(sex) %in% c("female", "f"), -3.3407, NA))
  if (method=="Faucon") {
    # Faucon Formula
    ecv = (alpha + 0.1393*weight_kg + 0.0455*height_cm + 0.0125*age)*1000 # in mL
  } else if (method == "Granerus") {
    # Granerus Formula
    if (tolower(sex) %in% c("male", "m")) {
      ecv = 166*weight_kg + 2490 # in mL
    } else if (tolower(sex) %in% c("female", "f")) {
      ecv = 95*weight_kg + 6170} # in mL
  }
  return(ecv) # mL
}

