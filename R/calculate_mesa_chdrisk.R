#' Calculate MESA CHD Risk (with CAC score)
#'
#' Estimates 10-year risk of ASCVD events as described in the MESA study. Full
#' description provided in \href{http://www.ncbi.nlm.nih.gov/pubmed/26449133}{McClelland  et al.,
#' JACC 2015}. An online calculator can be found at the \href{https://internal.mesa-nhlbi.org/about/procedures/tools/mesa-score-risk-calculator}{MESA home website}. CHD Risk Scorewas  defined as a composite outcome including nonfatal myocardial infarctions, resuscitated cardiac arrest, probable angina, definite angina followed by revascularization, and fatal CHD.
#'
#' @param age Age in years
#' @param gender Gender, (Female/Male)
#' @param race Race, one of (White, Black, Hispanic, Asian)
#' @param cac CAC score (Agatston score). If not available, set to NA.
#' @param Tc Total Cholesterol (mg/dL). converted to mmol/L internally
#' @param HDL HDL  Cholesterol (mg/dL). converted to mmol/L internally
#' @param SBP Systolic Blood Pressure, mmHg
#' @param current_smoker current_smoker, T/F
#' @param using_antihypertensive_medication HTN medication use, T/F
#' @param using_lipidloweringagent Lipid Lowering Medication use, T/F
#' @param diabetes Diabetes, T/F
#' @param MI_fmhx Family history of MI (in first degree relative), T/F
#' @param chol_units default = mg/dL; cholesterol units not in mg/dL then define
#'   here for conversion
#'
#' @return 10-year MESA CHD Risk (percent) in Dataframe format
#' @export
#'
#' @examples
#' calculate_mesa_chdrisk(age=50, cac = 0,
#'                        gender = "male",
#'                        race = "white",
#'                        Tc=200, 
#'                        HDL=34, 
#'                        SBP=125,  
#'                        current_smoker=FALSE,
#'                        using_antihypertensive_medication=FALSE,
#'                        using_lipidloweringagent=FALSE,
#'                        diabetes=FALSE,
#'                        MI_fmhx=FALSE)
#' 
#' calculate_mesa_chdrisk(age=60, # case 
#'                        cac = 220,
#'                        gender = "male",
#'                        race = "white",
#'                        Tc=200, 
#'                        HDL=45, 
#'                        SBP=126,  
#'                        current_smoker=FALSE,
#'                        using_antihypertensive_medication=FALSE,
#'                        using_lipidloweringagent=FALSE,
#'                        diabetes=FALSE,
#'                        MI_fmhx=TRUE)
#' calculate_mesa_chdrisk(age=60, # average healthy 60yo
#'                        cac = NA,
#'                        gender = "male",
#'                        race = "white",
#'                        Tc=192, 
#'                        HDL=45, 
#'                        SBP=123,  
#'                        current_smoker=FALSE,
#'                        using_antihypertensive_medication=FALSE,
#'                        using_lipidloweringagent=FALSE,
#'                        diabetes=FALSE,
#'                        MI_fmhx=TRUE)

calculate_mesa_chdrisk <- function(age, gender, race,
                                cac=NA,
                                Tc, HDL, SBP,  
                                current_smoker=FALSE,
                                using_antihypertensive_medication=FALSE,
                                using_lipidloweringagent=FALSE,
                                diabetes=FALSE,
                                MI_fmhx=FALSE,
                                chol_units = "mg/dL") {
  Tc = tabletools::convert_cholesterol_to_mgdl(Tc, cholesterol_units = chol_units)
  HDL = tabletools::convert_cholesterol_to_mgdl(HDL, cholesterol_units = chol_units)
  # validate gender
  gender_m = as.numeric(tolower(handle_sex(gender)) == "male")
  
  # validate race; use White/unadjusted if missing
  race = ifelse(is.na(handle_race(race)), "White", handle_race(race))
  
  # validate T/F
  current_smoker = as.numeric(as.logical(current_smoker))
  using_antihypertensive_medication = as.numeric(as.logical(using_antihypertensive_medication))
  using_lipidloweringagent = as.numeric(as.logical(using_lipidloweringagent))
  diabetes = as.numeric(as.logical(diabetes))
  MI_fmhx = as.numeric(as.logical(MI_fmhx))
  
  # Base Mesa Risk (no CAC)  
  mesa_base = mesa10yrcoeff[mesa10yrcoeff$risk=="mesa_10yr", ]

  Terms = 
    {(age * mesa_base$age) +
        (gender_m * mesa_base$gender_m)  +
        # default/base case is White; modified if other:
        ((race == "Black") * mesa_base$race_black)  +
        ((race == "Asian") * mesa_base$race_asian)  +
        ((race == "Hispanic") * mesa_base$race_hispanic)  +
        (diabetes * mesa_base$diabetes) +
        (current_smoker * mesa_base$current_smoker) +
        (Tc * mesa_base$Tc)  +
        (HDL * mesa_base$HDL)  +
        (using_lipidloweringagent * mesa_base$using_lipidloweringagent)  +
        (SBP * mesa_base$SBP)  +
        (using_antihypertensive_medication * mesa_base$using_antihypertensive_medication) +
        (MI_fmhx * mesa_base$MI_fmhx)
    }
  mesa_risk_10yr =  100 * (1 - 0.99963^(exp(Terms)))
  
  # Mesa Risk with CAC  
  mesa_cac = mesa10yrcoeff[mesa10yrcoeff$risk=="mesa_cac_10yr", ]
  Terms_cac = 
    {(age * mesa_cac$age) +
        (gender_m * mesa_cac$gender_m)  +
        # default/base case is White; modified if other:
        ((race == "Black") * mesa_cac$race_black)  +
        ((race == "Asian") * mesa_cac$race_asian)  +
        ((race == "Hispanic") * mesa_cac$race_hispanic)  +
        (diabetes * mesa_cac$diabetes) +
        (current_smoker * mesa_cac$current_smoker) +
        (Tc * mesa_cac$Tc)  +
        (HDL * mesa_cac$HDL)  +
        (using_lipidloweringagent * mesa_cac$using_lipidloweringagent)  +
        (SBP * mesa_cac$SBP)  +
        (using_antihypertensive_medication * mesa_cac$using_antihypertensive_medication) +
        (MI_fmhx * mesa_cac$MI_fmhx) +
        (log(cac + 1) * mesa_cac$cac)
    }
  mesa_risk_cac_10yr =  100 * (1 - 0.99833^(exp(Terms_cac)))

return(data.frame(#Terms, 
                  mesa_risk_10yr,
                  # Terms_cac, 
                  mesa_risk_cac_10yr))
}

