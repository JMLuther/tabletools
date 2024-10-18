#'Calculate AHA PREVENT Risk (full model)
#'
#'Uses the PREVENT Full model incorporating urine albumin, A1C, and SDI if
#'available into risk estimate: PREVENT (AHA Predicting Risk of CVD Events). The
#'PREVENT equations enable 10 and 30-year risk estimates for total CVD
#'(composite of atherosclerotic CVD and heart failure), ASCVD (atherosclerotic
#'CVD), Heart failure (HF), Coronary Artery Disease (CAD), and Stroke. Details
#' provided in \href{https://pubmed.ncbi.nlm.nih.gov/37947085/}{Khan et al.,
#' Circulation. 2024}. This model incorporates urine albumin, A1C and socioeconomic risk (by Zip code) if available. SDI lookup by zipcode is slightly off in SDI estimate- enter the SDI decile (1-10) for most accurate results.
#'
#'
#'@param risk Desired Risk Calculation ("cvd", "ascvd", "hf", "cad", "stroke")
#'@param gender Gender, (Female/Male)
#'@param age Age in years
#'@param Tc Total Cholesterol (mg/dL). converted to mmol/L internally
#'@param HDL HDL  Cholesterol (mg/dL). converted to mmol/L internally
#'@param SBP Systolic Blood Pressure, mmHg
#'@param eGFR estimated GFR (ml/min/1.73m2)
#'@param BMI Body Mass Index (kg/m2), used only in HF estimate
#'@param UACR Urine albumin/creatinine ratio (mg/g creatinine)
#'@param HbA1c Hemoglobin A1C (percent)
#'@param SDI Social Deprivation Index (SDI), decile
#'@param zipcode Zipcode to use for SDI lookup, if SDI not provided
#'@param year Year to use for SDI lookup, if SDI not provided
#'@param current_smoker current_smoker, T/F
#'@param using_antihypertensive_medication HTN medication use, T/F
#'@param using_statin Statin use, T/F
#'@param diabetes Diabetes, T/F
#'@param chol_units default = mg/dL; cholesterol units not in mg/dL then define
#'  here for conversion
#'
#'@return 10- and 30-year Risk (percent) in Dataframe format
#'@export
#'
#' @examples
#'
#'
#'  calculate_prevent_risk_full(risk="cvd",gender="Female", age=50, Tc=200, HDL=45, SBP=160, eGFR=90,
#'                              UACR = 40, HbA1c = 7.5, SDI = 8,
#'                              using_antihypertensive_medication = TRUE, diabetes = TRUE)
#'  calculate_prevent_risk_full(risk="ascvd",gender="female", age=50, Tc=200, HDL=45, SBP=160, eGFR=90,
#'                              UACR = 40, HbA1c = 7.5, SDI = 8,
#'                              using_antihypertensive_medication = TRUE, diabetes = TRUE)
#'  calculate_prevent_risk_full(risk="hf",gender="female", age=50, Tc=200, HDL=45, SBP=160, eGFR=90, BMI=35,
#'                              UACR = 40, HbA1c = 7.5, SDI = 8,
#'                              using_antihypertensive_medication = TRUE, diabetes = TRUE)
#'  calculate_prevent_risk_full(risk="cad",gender="female", age=50, Tc=200, HDL=45, SBP=160, eGFR=90,
#'                              UACR = 40, HbA1c = 7.5, SDI = 8,
#'                              using_antihypertensive_medication = TRUE, diabetes = TRUE)
#'  calculate_prevent_risk_full(risk="stroke",gender="female", age=50, Tc=200, HDL=45, SBP=160, eGFR=90,
#'                              UACR = 40, HbA1c = 7.5, SDI = 8,
#'                              using_antihypertensive_medication = TRUE, diabetes = TRUE)
#'
#'  # men
#'  calculate_prevent_risk_full(risk="cvd",gender="male", age=50, Tc=200, HDL=45, SBP=160, eGFR=90,
#'                              UACR = 40, HbA1c = 7.5, SDI = 8,
#'                              using_antihypertensive_medication = TRUE, diabetes = TRUE)
#'  calculate_prevent_risk_full(risk="ascvd",gender="male", age=50, Tc=200, HDL=45, SBP=160, eGFR=90,
#'                              UACR = 40, HbA1c = 7.5, SDI = 8,
#'                              using_antihypertensive_medication = TRUE, diabetes = TRUE)
#'  calculate_prevent_risk_full(risk="hf",gender="male", age=50, Tc=200, HDL=45, SBP=160, eGFR=90, BMI=35,
#'                              UACR = 40, HbA1c = 7.5, SDI = 8,
#'                              using_antihypertensive_medication = TRUE, diabetes = TRUE)
#'  calculate_prevent_risk_full(risk="cad",gender="male", age=50, Tc=200, HDL=45, SBP=160, eGFR=90,
#'                              UACR = 40, HbA1c = 7.5, SDI = 8,
#'                              using_antihypertensive_medication = TRUE, diabetes = TRUE)
#'  calculate_prevent_risk_full(risk="stroke",gender="male", age=50, Tc=200, HDL=45, SBP=160, eGFR=90,
#'                              UACR = 40, HbA1c = 7.5, SDI = 8,
#'                              using_antihypertensive_medication = TRUE, diabetes = TRUE)
#'
#' # Lookup SDI by zipcode; defaults to year 2019
#'  calculate_prevent_risk_full(risk="stroke",gender="male", age=50, Tc=200, HDL=45, SBP=160, eGFR=90,
#'                              UACR = 40, HbA1c = 7.5, SDI = NA, zipcode=37220,
#'                              using_antihypertensive_medication = TRUE, diabetes = TRUE)
#'  calculate_prevent_risk_full(risk="stroke",gender="male", age=50, Tc=200, HDL=45, SBP=160, eGFR=90,
#'                              UACR = 40, HbA1c = 7.5, SDI = 1,
#'                              using_antihypertensive_medication = TRUE, diabetes = TRUE)
#'
#'  library(tidyverse)
#'  df <- crossing(
#'    gender=c("female", "male"),
#'    age=50, Tc=200, HDL=45, eGFR=90, BMI=35,
#'    using_antihypertensive_medication = TRUE, diabetes = TRUE,
#'    HbA1c = 7.5, SDI = 10,
#'    SBP= seq(130,180,by=10),
#'    UACR=seq(0,300, by=50)
#'  )
#'  # seq(0,3000, by=50)
#'  df_risk <-
#'    df |>
#'    rowwise() |>
#'    mutate(calculate_prevent_risk_full(risk="cvd", gender, age, Tc, HDL, SBP, eGFR, BMI,
#'                                       UACR, HbA1c, SDI,
#'                                       using_antihypertensive_medication = using_antihypertensive_medication,
#'                                       diabetes=diabetes),
#'           calculate_prevent_risk_full(risk="ascvd", gender, age, Tc, HDL, SBP, eGFR, BMI,
#'                                       UACR, HbA1c, SDI,
#'                                       using_antihypertensive_medication = using_antihypertensive_medication,
#'                                       diabetes=diabetes),
#'           calculate_prevent_risk_full(risk="hf", gender, age, Tc, HDL, SBP, eGFR, BMI,
#'                                       UACR, HbA1c, SDI,
#'                                       using_antihypertensive_medication = using_antihypertensive_medication,
#'                                       diabetes=diabetes),
#'           calculate_prevent_risk_full(risk="cad", gender, age, Tc, HDL, SBP, eGFR, BMI,
#'                                       UACR, HbA1c, SDI,
#'                                       using_antihypertensive_medication = using_antihypertensive_medication,
#'                                       diabetes=diabetes),
#'           calculate_prevent_risk_full(risk="stroke", gender, age, Tc, HDL, SBP, eGFR, BMI,
#'                                       UACR, HbA1c, SDI,
#'                                       using_antihypertensive_medication = using_antihypertensive_medication,
#'                                       diabetes=diabetes)
#'    ) 
#' df_risk

calculate_prevent_risk_full <- function(risk, gender, 
                                   age, Tc, HDL, SBP, eGFR, 
                                   BMI=NA, UACR=NA, HbA1c=NA, 
                                   SDI=NA, zipcode=NA, year=2019,
                                   current_smoker=FALSE,
                                   using_antihypertensive_medication=FALSE,
                                   using_statin=FALSE,
                                   diabetes=FALSE,
                                   chol_units = "mg/dL") {
  
  BMI = ifelse(is.na(BMI), 0, BMI) # used only in HF calculations
  SDI =ifelse(!is.na(SDI), SDI,
              ifelse(!is.na(zipcode), sdi_decile({{zipcode}}, {{year}}), NA))
  SDI_missing =  ifelse(is.na(SDI) & is.na(zipcode), TRUE, FALSE)
  SDI4_6 = SDI>=4 & SDI<7
  SDI7_10 = SDI>=7
  UACR_missing = ifelse(is.na(UACR), TRUE, FALSE)
  HbA1c_missing = ifelse(is.na(HbA1c), TRUE, FALSE)
  Tc = ifelse(chol_units == "mg/dL", Tc*0.02586, Tc) # convert to mmol/L
  HDL = ifelse(chol_units == "mg/dL", HDL*0.02586, HDL)  # convert to mmol/L

  # validate risk request
  risk = tolower(risk)
  risk_query <- 
    ifelse(risk%in% c("cvd", "cardiovascular disease", "cardiovascular"), "cvd",
           ifelse(risk%in% c("ascvd", "atherosclerotic cvd"), "ascvd",
                  ifelse(risk%in% c("hf", "chf", "heart failure", "ahf"), "hf",
                         ifelse(risk%in% c("cad", "coronary artery disease", "mi", "myocardial infarction", "heart attack"), "cad",
                                ifelse(risk%in% c("stroke", "cva"), "stroke", NA)))))
  # validate gender
  gender_query= tolower(handle_sex(gender))

  # 10 YEAR BASE MODEL  
  cf10 <- cfs_full10yr[cfs_full10yr$risk=={{risk_query}} & cfs_full10yr$gender=={{gender_query}}, ]
  
  # construct the model
  log_odds10 = 
    cf10$constant + 
    cf10$age*(age-55) /10 + # AGE 
    cf10$nonHDL*(Tc-HDL-3.5) +  # NON-HDL CHOL
    cf10$HDL*(HDL-1.3) /0.3 +
    cf10$sbp_under110*(min(SBP, 110)-110) /20 +
    cf10$sbp_over110*(max(SBP, 110)-130) /20 + 
    cf10$diabetes*(diabetes) + 
    cf10$current_smoker*(current_smoker) + 
    # BMI used in HF calc only
    cf10$bmi_under30*(min(BMI, 30)-25) / 5 + 
    cf10$bmi_over30*(max(BMI, 30)-30) / 5 + 
    cf10$eGFR_under60*(min(eGFR, 60)-60) / -15 + 
    cf10$eGFR_over60*(max(eGFR, 60)-90) / -15 + 
    cf10$using_antihypertensive_medication*(using_antihypertensive_medication)+
    cf10$using_statin*(using_statin)+
    cf10$sbp_treated_over110*(using_antihypertensive_medication)*(max(SBP, 110)-130) /20 + 
    cf10$treated_nonHDL*(using_statin)*(Tc-HDL-3.5) +
    cf10$age_nonHDL_int*(age-55) /10*(Tc-HDL-3.5) + 
    cf10$age_HDL_int*(age-55) /10*(HDL-1.3) /0.3 +
    cf10$age_sbp_int*(age-55) /10*(max(SBP, 110)-130)/20 +
    cf10$age_diabetes_int*(age-55) /10*(diabetes) +
    cf10$age_smoking_int*(age-55) /10*(current_smoker) +
    cf10$age_bmi_int*(age-55)/10*(max(BMI, 30)-30)/5 +
    cf10$age_egfr_int*(age-55) /10*(min(eGFR, 60)-60) / -15 +
    ifelse(SDI_missing, cf10$sdi_missing, 
           ifelse(SDI4_6, cf10$sdi_4_6,
                  ifelse(SDI7_10, cf10$sdi__10, 0))) +
    ifelse(UACR_missing, cf10$acr_missing, cf10$ln_acr*log(UACR)) +
    ifelse(HbA1c_missing, cf10$a1c_missing, 
           ifelse(diabetes, cf10$a1c_dm*(HbA1c-5.3),
                  ifelse(!diabetes, cf10$a1c_nondm*(HbA1c-5.3)))) 

  # # 30 YEAR BASE MODEL  
  cf30 <- cfs_full30yr[cfs_full30yr$risk=={{risk_query}} & cfs_full30yr$gender=={{gender_query}}, ]

  # construct the model
  log_odds30 = 
    cf30$constant + 
    cf30$age*(age-55) /10 + # AGE 
    cf30$age_sq*((age-55) /10)^2 + # AGE 
    cf30$nonHDL*(Tc-HDL-3.5) +  # NON-HDL CHOL
    cf30$HDL*(HDL-1.3) /0.3 +
    cf30$sbp_under110*(min(SBP, 110)-110) /20 +
    cf30$sbp_over110*(max(SBP, 110)-130) /20 + 
    cf30$diabetes*(diabetes) + 
    cf30$current_smoker*(current_smoker) + 
    # BMI used in HF calc only
    cf30$bmi_under30*(min(BMI, 30)-25) / 5 + 
    cf30$bmi_over30*(max(BMI, 30)-30) / 5 + 
    cf30$eGFR_under60*(min(eGFR, 60)-60) / -15 + 
    cf30$eGFR_over60*(max(eGFR, 60)-90) / -15 + 
    cf30$using_antihypertensive_medication*(using_antihypertensive_medication)+
    cf30$using_statin*(using_statin)+
    cf30$sbp_treated_over110*(using_antihypertensive_medication)*(max(SBP, 110)-130) /20 + 
    cf30$treated_nonHDL*(using_statin)*(Tc-HDL-3.5) +
    cf30$age_nonHDL_int*(age-55) /10*(Tc-HDL-3.5) + 
    cf30$age_HDL_int*(age-55) /10*(HDL-1.3) /0.3 +
    cf30$age_sbp_int*(age-55) /10*(max(SBP, 110)-130)/20 +
    cf30$age_diabetes_int*(age-55) /10*(diabetes) +
    cf30$age_smoking_int*(age-55) /10*(current_smoker) +
    cf30$age_bmi_int*(age-55)/10*(max(BMI, 30)-30)/5 +
    cf30$age_egfr_int*(age-55) /10*(min(eGFR, 60)-60) / -15 +
    ifelse(SDI_missing, cf30$sdi_missing, 
           ifelse(SDI4_6, cf30$sdi_4_6,
                  ifelse(SDI7_10, cf30$sdi__10, 0))) +
    ifelse(UACR_missing, cf30$acr_missing, cf30$ln_acr*log(UACR)) +
    ifelse(HbA1c_missing, cf30$a1c_missing, 
           ifelse(diabetes, cf30$a1c_dm*(HbA1c-5.3),
                  ifelse(!diabetes, cf30$a1c_nondm*(HbA1c-5.3)))) 
  
  Risk10 = exp(log_odds10) / (1 + exp(log_odds10))
  Risk30 = exp(log_odds30) / (1 + exp(log_odds30))
  res = data.frame(Risk10*100, 
                   Risk30*100)
  names(res) <- paste0(risk, c("_10yr", "_30yr"))
  return(res)
  
}



