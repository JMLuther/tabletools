#' Calculate AHA PREVENT Risk
#'
#' Uses the PREVENT Base model (sex-specific, race-free) risk equations: PREVENT
#' (AHA Predicting Risk of CVD Events). The PREVENT equations enable 10 and
#' 30-year risk estimates for total CVD (composite of atherosclerotic CVD and
#' heart failure), ASCVD (atherosclerotic CVD), Heart failure (HF), Coronary
#' Artery Disease (CAD), and Stroke. Details
#' provided in \href{https://pubmed.ncbi.nlm.nih.gov/37947085/}{Khan et al.,
#' Circulation. 2024}. Extended models incorporating urine albumin, A1C and socioeconomic risk (by Zip code) are available.
#'
#' Allowable range for Risk variables are:
#'
#' *  Age 30-79
#' *  Total Cholesterol 130-320 mg/dL
#' *  HDL Cholesterol 20-100 mg/dL
#' *  SBP 90-200 mmHg
#' *  BMI 18.5 - 40 mg/m^2
#' *  eGFR 15-150 mL/min/1.73m^2
#' *  Urine albumin/Creatinine 0.1-2500 mg/g Creatinine
#'
#' @param risk Desired Risk Calculation ("cvd", "ascvd", "hf", "cad", "stroke")
#' @param gender Gender, (Female/Male)
#' @param age Age in years
#' @param Tc Total Cholesterol (mg/dL). converted to mmol/L internally
#' @param HDL HDL  Cholesterol (mg/dL). converted to mmol/L internally
#' @param SBP Systolic Blood Pressure, mmHg
#' @param eGFR estimated GFR (ml/min/1.73m2)
#' @param BMI Body Mass Index (kg/m2), used only in HF estimate
#' @param current_smoker current_smoker, T/F
#' @param using_antihypertensive_medication HTN medication use, T/F
#' @param using_statin Statin use, T/F
#' @param diabetes Diabetes, T/F
#' @param chol_units default = mg/dL; cholesterol units not in mg/dL then define
#'   here for conversion
#'
#' @return 10- and 30-year Risk (percent) in Dataframe format
#' @export
#'
#' @examples
#'
#' # Example results below have been validated against original publication (XLSX file with calc check)
#' # women
#'calculate_prevent_risk(risk="cvd",gender="Female", age=50, Tc=200, HDL=45, SBP=160, eGFR=90,
#'                       using_antihypertensive_medication = TRUE, diabetes = TRUE)
#'calculate_prevent_risk(risk="ascvd",gender="female", age=50, Tc=200, HDL=45, SBP=160, eGFR=90,
#'                       using_antihypertensive_medication = TRUE, diabetes = TRUE)
#'calculate_prevent_risk(risk="hf",gender="female", age=50, Tc=200, HDL=45, SBP=160, eGFR=90, BMI=35,
#'                       using_antihypertensive_medication = TRUE, diabetes = TRUE)
#'calculate_prevent_risk(risk="cad",gender="female", age=50, Tc=200, HDL=45, SBP=160, eGFR=90,
#'                       using_antihypertensive_medication = TRUE, diabetes = TRUE)
#'calculate_prevent_risk(risk="stroke",gender="female", age=50, Tc=200, HDL=45, SBP=160, eGFR=90,
#'                       using_antihypertensive_medication = TRUE, diabetes = TRUE)
#'
#'# men
#'calculate_prevent_risk(risk="cvd",gender="male", age=50, Tc=200, HDL=45, SBP=160, eGFR=90,
#'                       using_antihypertensive_medication = TRUE, diabetes = TRUE)
#'calculate_prevent_risk(risk="ascvd",gender="male", age=50, Tc=200, HDL=45, SBP=160, eGFR=90,
#'                       using_antihypertensive_medication = TRUE, diabetes = TRUE)
#'calculate_prevent_risk(risk="hf",gender="male", age=50, Tc=200, HDL=45, SBP=160, eGFR=90, BMI=35,
#'                       using_antihypertensive_medication = TRUE, diabetes = TRUE)
#'calculate_prevent_risk(risk="cad",gender="male", age=50, Tc=200, HDL=45, SBP=160, eGFR=90,
#'                       using_antihypertensive_medication = TRUE, diabetes = TRUE)
#'calculate_prevent_risk(risk="stroke",gender="male", age=50, Tc=200, HDL=45, SBP=160, eGFR=90,
#'                       using_antihypertensive_medication = TRUE, diabetes = TRUE)

calculate_prevent_risk <- function(
  risk,
  gender,
  age,
  Tc,
  HDL,
  SBP,
  eGFR,
  BMI = NA,
  current_smoker = FALSE,
  using_antihypertensive_medication = FALSE,
  using_statin = FALSE,
  diabetes = FALSE,
  chol_units = "mg/dL"
) {
  # Replace values out of range with NA_real_:
  age = ifelse(age < 30, NA_real_, ifelse(age > 79, NA_real_, age))
  Tc = ifelse(Tc < 130, NA_real_, ifelse(Tc > 320, NA_real_, Tc))
  HDL = ifelse(HDL < 20, NA_real_, ifelse(HDL > 100, NA_real_, HDL))
  SBP = ifelse(SBP < 90, NA_real_, ifelse(SBP > 200, NA_real_, SBP))
  eGFR = ifelse(eGFR < 15, NA_real_, ifelse(eGFR > 150, NA_real_, eGFR))
  BMI = ifelse(BMI < 18.5, NA_real_, ifelse(BMI > 40, NA_real_, BMI))

  BMI = ifelse(is.na(BMI), 0, BMI) # used only in HF calculations
  Tc = ifelse(chol_units == "mg/dL", Tc * 0.02586, Tc)
  # tabletools::convert_cholesterol_to_mM(Tc, cholesterol_units = chol_units)
  HDL = ifelse(chol_units == "mg/dL", HDL * 0.02586, HDL)
  # tabletools::convert_cholesterol_to_mM(HDL, cholesterol_units = chol_units)

  # validate risk request
  risk = tolower(risk)
  risk_query <-
    ifelse(
      risk %in% c("cvd", "cardiovascular disease", "cardiovascular"),
      "cvd",
      ifelse(
        risk %in% c("ascvd", "atherosclerotic cvd"),
        "ascvd",
        ifelse(
          risk %in% c("hf", "chf", "heart failure", "ahf"),
          "hf",
          ifelse(
            risk %in%
              c(
                "cad",
                "coronary artery disease",
                "mi",
                "myocardial infarction",
                "heart attack"
              ),
            "cad",
            ifelse(risk %in% c("stroke", "cva"), "stroke", NA)
          )
        )
      )
    )
  # validate gender
  gender_query = tolower(handle_sex(gender))

  # 10 YEAR BASE MODEL
  # model coeffs stored in data frame tabletools:::cfs_base10yr
  cf10 <- cfs_base10yr[
    cfs_base10yr$risk == {{ risk_query }} &
      cfs_base10yr$gender == {{ gender_query }},
  ]

  # construct the model
  log_odds10 =
    cf10$constant +
    cf10$age * (age - 55) / 10 + # AGE
    cf10$nonHDL * (Tc - HDL - 3.5) + # NON-HDL CHOL
    cf10$HDL * (HDL - 1.3) / 0.3 +

    cf10$sbp_under110 * (min(SBP, 110) - 110) / 20 +
    cf10$sbp_over110 * (max(SBP, 110) - 130) / 20 +

    cf10$diabetes * (diabetes) +
    cf10$current_smoker * (current_smoker) +

    # BMI used in HF calc only
    cf10$bmi_under30 * (min(BMI, 30) - 25) / 5 +
    cf10$bmi_over30 * (max(BMI, 30) - 30) / 5 +

    cf10$eGFR_under60 * (min(eGFR, 60) - 60) / -15 +
    cf10$eGFR_over60 * (max(eGFR, 60) - 90) / -15 +

    cf10$using_antihypertensive_medication *
      (using_antihypertensive_medication) +
    cf10$using_statin * (using_statin) +
    cf10$sbp_treated_over110 *
      (using_antihypertensive_medication) *
      (max(SBP, 110) - 130) /
      20 +
    cf10$treated_nonHDL * (using_statin) * (Tc - HDL - 3.5) +
    cf10$age_nonHDL_int * (age - 55) / 10 * (Tc - HDL - 3.5) +
    cf10$age_HDL_int * (age - 55) / 10 * (HDL - 1.3) / 0.3 +
    cf10$age_sbp_int * (age - 55) / 10 * (max(SBP, 110) - 130) / 20 +
    cf10$age_diabetes_int * (age - 55) / 10 * (diabetes) +
    cf10$age_smoking_int * (age - 55) / 10 * (current_smoker) +
    cf10$age_bmi_int * (age - 55) / 10 * (max(BMI, 30) - 30) / 5 +
    cf10$age_egfr_int * (age - 55) / 10 * (min(eGFR, 60) - 60) / -15

  # 30 YEAR BASE MODEL
  # model coeffs stored in data frame tabletools:::cfs_base30yr
  cf30 <- cfs_base30yr[
    cfs_base30yr$risk == {{ risk_query }} &
      cfs_base30yr$gender == {{ gender_query }},
  ]

  # construct the model
  log_odds30 =
    cf30$constant +
    cf30$age * (age - 55) / 10 + # AGE
    cf30$age_sq * ((age - 55) / 10)^2 + # AGE
    cf30$nonHDL * (Tc - HDL - 3.5) + # NON-HDL CHOL
    cf30$HDL * (HDL - 1.3) / 0.3 +

    cf30$sbp_under110 * (min(SBP, 110) - 110) / 20 +
    cf30$sbp_over110 * (max(SBP, 110) - 130) / 20 +

    cf30$diabetes * (diabetes) +
    cf30$current_smoker * (current_smoker) +

    # BMI used in HF calc only
    cf30$bmi_under30 * (min(BMI, 30) - 25) / 5 +
    cf30$bmi_over30 * (max(BMI, 30) - 30) / 5 +

    cf30$eGFR_under60 * (min(eGFR, 60) - 60) / -15 +
    cf30$eGFR_over60 * (max(eGFR, 60) - 90) / -15 +

    cf30$using_antihypertensive_medication *
      (using_antihypertensive_medication) +
    cf30$using_statin * (using_statin) +
    cf30$sbp_treated_over110 *
      (using_antihypertensive_medication) *
      (max(SBP, 110) - 130) /
      20 +
    cf30$treated_nonHDL * (using_statin) * (Tc - HDL - 3.5) +
    cf30$age_nonHDL_int * (age - 55) / 10 * (Tc - HDL - 3.5) +
    cf30$age_HDL_int * (age - 55) / 10 * (HDL - 1.3) / 0.3 +
    cf30$age_sbp_int * (age - 55) / 10 * (max(SBP, 110) - 130) / 20 +
    cf30$age_diabetes_int * (age - 55) / 10 * (diabetes) +
    cf30$age_smoking_int * (age - 55) / 10 * (current_smoker) +
    cf30$age_bmi_int * (age - 55) / 10 * (max(BMI, 30) - 30) / 5 +
    cf30$age_egfr_int * (age - 55) / 10 * (min(eGFR, 60) - 60) / -15

  Risk10 = exp(log_odds10) / (1 + exp(log_odds10))
  Risk30 = exp(log_odds30) / (1 + exp(log_odds30))
  res = data.frame(Risk10 * 100, Risk30 * 100)
  names(res) <- paste0(risk, c("_10yr", "_30yr"))
  return(res)
}
