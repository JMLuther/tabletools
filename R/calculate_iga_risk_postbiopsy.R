#'International IgAN Prediction Tool post-biopsy
#'
#'@description
#'
#'Post-Biopsy Risk estimator for Kidney outcomes in patients with IgA
#'Nephropathy, using the Oxford IgaN MEST Histology Score, estimated at a
#'time-point 1 year after biopsy using the formulas in the manuscript by
#'\href{https://pubmed.ncbi.nlm.nih.gov/35490842/}{Barbour et al, Kidney
#'International 2022}. The calculations have been validated against an available
#'online calculator at
#'\href{https://qxmd.com/calculate/calculator_839/international-igan-prediction-tool-post-biopsy-adults#}{Calculate
#'by QxMD}. Note if Race does not fit into the categories of "Chinese", "Japanese", or "Caucasian" then it is recommended to use the race-free equation.
#'
#'`calculate_iga_risk_postbiopsy()` is the full Race-based model
#'`calculate_iga_risk_postbiopsy_norace()` Provides an estimate when race is not
#'known or if race is not well matched by Caucasian, Chinese, or Japanese
#'race/ethnicity
#'
#'
#'@param egfr estimated GFR (ml/min/1.73m2)
#'@param sbp Systolic BP (mmHg)
#'@param dbp Diastolic BP (mmHg)
#'@param proteinuria Proteinuria (g/day)
#'@param age Age (years)
#'@param mest_M MEST score, Mesangial (M) hypercellularity (0,1)
#'@param mest_E MEST score, Endocapillary (E) hypercellularity (0,1)
#'@param mest_S MEST score, Segmental Sclerosis (0,1)
#'@param mest_T MEST score, interstitial fibrosis/tubular atrophy (0,1,2)
#'@param time Time frame for Risk estimate (months; defaults to 5 years = 60
#'  months)
#'@param immunosuppression Immunosuppression use (TRUE/FALSE)
#'@param RASB Renin-angiotensin-aldosterone system blockade use (TRUE/FALSE)
#'@param race Race as `"Chinese", "Japanese", "Caucasian", or "Other"`
#'@param explanation Whether to output results as text description (TRUE/FALSE;
#'  defaults to FALSE)
#'
#'@return a single value of risk estimate for kidney outcome (>50% decline in
#'  eGFR or ESRD) in the desired time interval
#'@importFrom tabletools calculate_map 
#'@export 
#'
#' @examples
#'
#'
#' # Online calcresult = 51.42% vs 51.41738%
#' calculate_iga_risk_postbiopsy(egfr = 50, sbp=150, dbp=90,  proteinuria=1.2, age=30,
#'                               race="Chinese", RASB=T,
#'                               mest_M=1,mest_E=1,mest_S=1,mest_T=1, time=60)
#'
#' # Get text description of estimate
#' calculate_iga_risk_postbiopsy(egfr = 50, sbp=150, dbp=90,  proteinuria=1.2, age=30,
#'                               race="Chinese", RASB=T,
#'                               mest_M=1,mest_E=1,mest_S=1,mest_T=1, time=60,
#'                               explanation = T)
#'
#' # Online calcresult = 74.36% vs 74.3567%
#' calculate_iga_risk_postbiopsy(egfr = 50, sbp=150, dbp=90,  proteinuria=1.2, age=30,
#'                               race="Japanese", RASB=T,
#'                               mest_M=1,mest_E=1,mest_S=1,mest_T=1, time=60)
#'
#' # Online calcresult = 37.01% vs 37.01%
#' calculate_iga_risk_postbiopsy(egfr = 50, sbp=150, dbp=90,  proteinuria=1.2, age=30,
#'                               race="Caucasian", RASB=T,
#'                               mest_M=1,mest_E=1,mest_S=1,mest_T=1, time=60)
#'
#' # Online calcresult = 49.69% vs 27.98% # ONLINE uses no race if "Other" is specified
#' calculate_iga_risk_postbiopsy(egfr = 50, sbp=150, dbp=90,  proteinuria=1.2, age=30,
#'                               race="Other", RASB=T,
#'                               mest_M=1,mest_E=1,mest_S=1,mest_T=1, time=60)
#'
#' # Online calcresult = 49.69% vs 49.69%
#' calculate_iga_risk_postbiopsy_norace(egfr = 50, sbp=150, dbp=90,  proteinuria=1.2, age=30,
#'                                      RASB=T,
#'                                      mest_M=1,mest_E=1,mest_S=1,mest_T=1, time=60)
#' calculate_iga_risk_postbiopsy_norace(egfr = 50, sbp=150, dbp=90,  proteinuria=1.2, age=30,
#'                                      RASB=T,
#'                                      mest_M=1,mest_E=1,mest_S=1,mest_T=1, time=60,
#'                                      explanation = T)

calculate_iga_risk_postbiopsy <- function(egfr, sbp, dbp, proteinuria, age, 
                               mest_M, mest_E, mest_S, mest_T, 
                               time=60,
                               immunosuppression=FALSE, RASB=TRUE,
                               race="Caucasian", 
                               explanation=FALSE){

  # mean_bp = mean arterial pressure 
  # proteinuria = g/day (or urine protein/creatinine ratio)
  # age (years)
  # race as Chinese, Japanese, Caucasian, Other
  # Immunosuppression = T/F, immunosuppresion at or prior to timepoint
  # RASB = T/F, RAAS blockade/ACEi/ARB use at or prior to  timepoint
  # Oxford IgaN MEST Histology Score:
  # mesangial (M) and endocapillary (E) hypercellularity, segmental sclerosis (S), and interstitial fibrosis/tubular atrophy (T)
  # M1 = Mesangial (M) hypercellularity (0,1)
  # E1 = Endocapillary (E) hypercellularity (0,1)
  # S1 = Segmental Sclerosis (0,1)
  # T1 = interstitial fibrosis/tubular atrophy score of 1 (0,1)
  # T2 = interstitial fibrosis/tubular atrophy score of 2 (0,1)
  
  mean_ap = tabletools::calculate_map(sbp, dbp)
  M1 = as.numeric(mest_M == 1) # 0/1
  E1 = as.numeric(mest_E == 1) # 0/1
  S1 = as.numeric(mest_S == 1) # 0/1
  T1 = as.numeric(mest_T == 1) # 0/1; if T=2, then 0
  T2 = as.numeric(mest_T == 2)     # 0/1; 1 if T=2

  # RAce adjustment factor
  Chinese_race = as.numeric(grepl("Chin", race, ignore.case = T))
  Japanese_race = as.numeric(grepl("japan", race, ignore.case = T))
  Other_race =  as.numeric(grepl("other", race, ignore.case = T))

  #  LP = Linear predictor
  LP = -0.6505*(sqrt(egfr)-8.8) + 0.0021*(mean_ap-97) + 0.2516*(log(proteinuria) - 0.09) + 
    0.0043*((mean_ap*log(proteinuria)) - 8.73 ) + 
    0.2230*M1 + 0.0924*E1 + 0.0547*S1 + 0.1886*T1 + 0.2074*T2 - 
    0.1650*T1*log(proteinuria) - 0.2781*T2*log(proteinuria) -
    0.0255*(age-38) + #race_adj +
    0.4458*Chinese_race + 1.0798*Japanese_race -0.3425*Other_race +
    0.2337*RASB - 0.2879*immunosuppression
  
  s1=max((time-2.73)/38.41871,0)^3+ (46.94*max((time-240.86)/38.41871,0)^3-238.13*max((time-49.67)/38.41871,0)^3)/191.19
  S0=1.00130-0.00066339*time-0.00362*s1
  pred_risk = 1 - S0^exp(LP)

  # return(data.frame(pred_risk, s1, S0, LP, Chinese_race, Japanese_race, Other_race, mean_ap, M1, E1, S1, T1, T2))
  ifelse(!explanation, return(pred_risk),
         return(sprintf("Probability of an adverse kidney event (50%% eGFR decline or ESRD) at %1.0f months is %1.1f%%",
                        time, pred_risk*100))
         )
  
}

