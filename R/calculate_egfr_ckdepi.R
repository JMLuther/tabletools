#' Calculate estimated GFR by CKD-EPI 2021 equation
#'
#' @description Calculates estimated glomerular filtration rate (GFR) by several
#'   common methods. Converts weight to Kg and Height to cm if needed. Available
#'   methods for eGFR calculations include:
#'
#'   * CKD-EPI AS Equation (2021) incorporating only Age, Sex 
#'   * CKD-EPI ASR Equation (original, 2009) incorporating Age, Sex, Race
#'   * CKD-EPI Cystatin-C (not incorporated)
#'   * MDRD  
#'   * EKFC
#'   * KRS
#'   * Cockcroft-Gault 
#'
#' Online calculators can be used to compare reults for \href{https://www.kidney.org/professionals/kdoqi/gfr_calculator}{CKD-EPI AS (2021)},
#' \href{https://www.niddk.nih.gov/health-information/professionals/clinical-tools-patient-management/kidney-disease/laboratory-evaluation/glomerular-filtration-rate-calculators/historical}{CKD-EPI ASR}
#' 
#'
#' @param age Age, in years
#' @param sex Sex 
#' @param creatinine Serum Creatinine, mg/dL
#' @param race Race limited to either "White" or "Black"
#' @param version Equation version, either "2021" or "original"
#'
#' @returns a numeric vector with eGFR (ml/min/1.73m^2)
#' @export 
#'
#' @examples
#' # CKD-EPI 2021 version (new race-free creatinine-based equation)
#' # https://www.kidney.org/professionals/kdoqi/gfr_calculator
#' calculate_egfr_ckdepi(age=50, sex="Male", creatinine=0.6) # 
#' calculate_egfr_ckdepi(age=70, sex="Male", creatinine=0.8, version = "2021") # 95.2
#' calculate_egfr_ckdepi(age=70, sex="Female", creatinine=0.8, version = "2021") # 79.2
#' 
#' # use of race-incorporated version not recommended (ASR);
#' # if you want to use race, use version = "original".
#' calculate_egfr_ckdepi(age=70, sex="Female", creatinine=0.8, race="White", version = "2021") # error
#' calculate_egfr_ckdepi(age=70, sex="Female", creatinine=0.8, race="White", version = "original") # 74
#' 
#' # original CKD-EPI equation; see [NIH eGFR calculator](https://www.niddk.nih.gov/health-information/professionals/clinical-tools-patient-management/kidney-disease/laboratory-evaluation/glomerular-filtration-rate-calculators/historical)
#' calculate_egfr_ckdepi(age=70, sex="Male", creatinine=0.8, version = "original", race = "Black") # 104.2
#' calculate_egfr_ckdepi(age=70, sex="Male", creatinine=0.8, version = "original", race = "White") # 89.9
#' 
#' # CKD-EPI AS (2021) RESULTS; Race-free equation
#' # Note: do not use race in function call; results same regardless of race
#' # results verified against Table S11 in Inker NEJM 2021: eGFR (AS) new
#' df <-  data.frame(age=rep(c(rep(50,4),rep(75,4)),6),
#'                   sex = rep(c(rep("Male",8),rep("Female",8)),3),
#'                   race=c(rep("White",16),rep("Black",16),rep("White",16)),
#'                   creatinine = rep(c(0.6, 1, 1.5, 2), 6),
#'                   version = c(rep("2021", 16),rep("original", 32)))
#' df |>
#'   mutate(egfr = calculate_egfr_ckdepi(age=age, sex=sex, race=race, creatinine=creatinine, version=version),
#'          egfr_ckepi_original = calculate_egfr_ckdepi(age=age, sex=sex, race=race, creatinine=creatinine, version="original"),
#'          egfr_mdrd = calculate_egfr_mdrd(age, sex, race, creatinine),
#'          egfr_ekfc = calculate_egfr_ekfc(age, sex, creatinine),
#'          egfr_krs  = calculate_egfr_krs(age, sex, creatinine))

calculate_egfr_ckdepi <- Vectorize(
  function(age, sex, creatinine, race=NULL, version="2021") {
  sex = handle_sex(sex)
  # creat.f = switch(sex,
  #                  "Female" = as.character(cut(creatinine, breaks=c(0, 0.7, Inf), labels=c("<=0.7", ">0.7"))),
  #                  "Male"   = as.character(cut(creatinine, breaks=c(0, 0.9, Inf), labels=c("<=0.9", ">0.9"))))
  if (version=="2021" ) {
    k = switch(sex, "Female"=0.7, "Male"=0.9) # same for CKD-EPI and CKD-EPI Cystatin-C
    a = switch(sex, "Female" = -0.241, "Male"= -0.302)
    F.sex = switch(sex, "Female" = 1.012, "Male"=1)
    
    # CKD-EPI Age, Sex Equation (2021) (Age, Sex- refit Eq without Race) - Recommended, Default
    # results verified against publication, Inker NEJM table S11
    eGFR = 142*min((creatinine/k), 1)^a * max((creatinine/k), 1)^(-1.200) * 0.9938^age * F.sex
  # } else if (version=="2021" & !is.null(race)) { 
  #   stop("Race is not used in the 2021 CKD-EPI equation. use 'race=NULL' (default) or do not specify this argument.
  #        To use the Race-based equation, specify version='original' ")
  } else if (version=="original") { # 2009 CKD-EPI
    k = switch(sex, "Female" = 0.7, "Male"=0.9) # same for CKD-EPI and CKD-EPI Cystatin-C
    a = switch(sex, "Female" = -0.329, "Male"= -0.411)
    F.sex = switch(sex, "Female" = 1.018, "Male"=1)
    R = switch(race, "Black" = 1.159, "White"=1,
               stop("No race specified. use race='Black' or 'White'"))
    eGFR = 141*min((creatinine/k), 1)^a * max((creatinine/k), 1)^(-1.209) * 0.9929^age * F.sex * R
  }  else {eGFR = NA}
  return(eGFR)
  }
)
# calculate_egfr_ckdepi <- Vectorize(calculate_egfr_ckdepi_nonv)

# # examples for documentation that have been validated
# # Note nonvectorized form is ~10x faster and can be called if speed is an issue
# calculate_egfr_ckdepi_nonv(age=50, sex="Male", creatinine=0.6) # 118
# calculate_egfr_ckdepi(age=50, sex="Male", creatinine=0.6) # 118

# calculate_egfr_ckdepi(age=50, sex="Male", creatinine=1)   # 92
# calculate_egfr_ckdepi(age=50, sex="Male", creatinine=1.5) # 56
# calculate_egfr_ckdepi(age=50, sex="Male", creatinine=2.0) # 40
# 
# calculate_egfr_ckdepi(age=75, sex="Male", creatinine=0.6) # 101
# calculate_egfr_ckdepi(age=75, sex="Male", creatinine=1)   # 78
# calculate_egfr_ckdepi(age=75, sex="Male", creatinine=1.5) # 48
# calculate_egfr_ckdepi(age=75, sex="Male", creatinine=2.0) # 34
# 
# calculate_egfr_ckdepi(age=50, sex="Female", creatinine=0.6) # 109
# calculate_egfr_ckdepi(age=50, sex="Female", creatinine=1)   # 69
# calculate_egfr_ckdepi(age=50, sex="Female", creatinine=1.5) # 42
# calculate_egfr_ckdepi(age=50, sex="Female", creatinine=2.0) # 30
# 
# calculate_egfr_ckdepi(age=75, sex="Female", creatinine=0.6) # 94
# calculate_egfr_ckdepi(age=75, sex="Female", creatinine=1)   # 59
# calculate_egfr_ckdepi(age=75, sex="Female", creatinine=1.5) # 36
# calculate_egfr_ckdepi(age=75, sex="Female", creatinine=2.0) # 26
# 
# 
# # CKD-EPI ASR (2021) RESULTS; Race-based equation
# # this equation is equivalent to the original 2009 CKD-EPI equation
# # The CKD-EPI ASR-NB (2021) = "Non-Black" is equivalent to using race="White"
# # version = "original" allows specification of race = "White" or "Black"
# # results verified against Table S11 in Inker NEJM 2021: eGFR (ASR) current
# calculate_egfr_ckdepi(age=50, sex="Male", race="Black", creatinine=0.6, version = "original") # 135 (not 136)
# calculate_egfr_ckdepi(age=50, sex="Male", race="Black", creatinine=1, version = "original")   # 101
# calculate_egfr_ckdepi(age=50, sex="Male", race="Black", creatinine=1.5, version = "original") # 62
# calculate_egfr_ckdepi(age=50, sex="Male", race="Black", creatinine=2.0, version = "original") # 44
# 
# calculate_egfr_ckdepi(age=75, sex="Male", race="Black", creatinine=0.6, version = "original") # 113 (not 114)
# calculate_egfr_ckdepi(age=75, sex="Male", race="Black", creatinine=1, version = "original")   # 84 (not 85)
# calculate_egfr_ckdepi(age=75, sex="Male", race="Black", creatinine=1.5, version = "original") # 52
# calculate_egfr_ckdepi(age=75, sex="Male", race="Black", creatinine=2.0, version = "original") # 36 (not 37)
# 
# calculate_egfr_ckdepi(age=50, sex="Female", race="Black", creatinine=0.6, version = "original") # 123 (not 124)
# calculate_egfr_ckdepi(age=50, sex="Female", race="Black", creatinine=1, version = "original")   # 76
# calculate_egfr_ckdepi(age=50, sex="Female", race="Black", creatinine=1.5, version = "original") # 46 (not 47)
# calculate_egfr_ckdepi(age=50, sex="Female", race="Black", creatinine=2.0, version = "original") # 33
# 
# calculate_egfr_ckdepi(age=75, sex="Female", race="Black", creatinine=0.6, version = "original") # 103 (not 104)
# calculate_egfr_ckdepi(age=75, sex="Female", race="Black", creatinine=1, version = "original")   # 63 (not 64)
# calculate_egfr_ckdepi(age=75, sex="Female", race="Black", creatinine=1.5, version = "original") # 39
# calculate_egfr_ckdepi(age=75, sex="Female", race="Black", creatinine=2.0, version = "original") # 27 (not 28)
# 
# # race = "White" forces use of the CKD-EPI ASR-NB result
# calculate_egfr_ckdepi(age=50, sex="Male", race="White", creatinine=0.6, version = "original") # 117
# calculate_egfr_ckdepi(age=50, sex="Male", race="White", creatinine=1, version = "original")   # 87
# calculate_egfr_ckdepi(age=50, sex="Male", race="White", creatinine=1.5, version = "original") # 53 (not 54)
# calculate_egfr_ckdepi(age=50, sex="Male", race="White", creatinine=2.0, version = "original") # 38
# 
# calculate_egfr_ckdepi(age=75, sex="Male", race="White", creatinine=0.6, version = "original") # 98
# calculate_egfr_ckdepi(age=75, sex="Male", race="White", creatinine=1, version = "original")   # 73
# calculate_egfr_ckdepi(age=75, sex="Male", race="White", creatinine=1.5, version = "original") # 45
# calculate_egfr_ckdepi(age=75, sex="Male", race="White", creatinine=2.0, version = "original") # 32
# 
# calculate_egfr_ckdepi(age=50, sex="Female", race="White", creatinine=0.6, version = "original") # 106 (not 107)
# calculate_egfr_ckdepi(age=50, sex="Female", race="White", creatinine=1.0, version = "original")   # 65
# calculate_egfr_ckdepi(age=50, sex="Female", race="White", creatinine=1.5, version = "original") # 40
# calculate_egfr_ckdepi(age=50, sex="Female", race="White", creatinine=2.0, version = "original") # 29 (not 28)
# 
# calculate_egfr_ckdepi(age=75, sex="Female", race="White", creatinine=0.6, version = "original") # 88 (not 89)
# calculate_egfr_ckdepi(age=75, sex="Female", race="White", creatinine=1, version = "original")   # 55
# calculate_egfr_ckdepi(age=75, sex="Female", race="White", creatinine=1.5, version = "original") # 34
# calculate_egfr_ckdepi(age=75, sex="Female", race="White", creatinine=2.0, version = "original") # 24
