#' Gomez formulas for renal afferent/efferent resistance
#'
#' Calculates renal resistance measures using the formulas originally reported
#' by \href{https://pubmed.ncbi.nlm.nih.gov/14888692/}{Gomez}, referred to as
#' the "Gomez Formulas". For review of this method see
#' \href{https://pubmed.ncbi.nlm.nih.gov/27605583/}{Bjornstad}. For discussion
#' of the additionally reported A value see
#' \href{https://pubmed.ncbi.nlm.nih.gov/1860182/}{Kimura.}
#'
#' Assumptions imposed by Gomez equations are as follows: 
#' 1) Intrarenal vascular resistances are divided into afferent, postglomerular, 
#' and efferent 
#' 2) Hydrostatic pressures within the renal tubules, venules, Bowman’s space, and
#' interstitium (PBow) are in equilibrium of 10 mmHg 
#' 3) Glomerulus is  in filtration disequilibrium 
#' 4) Gross filtration coefficient KF is 0.0867 mL/s/mmHg given a normal kidney. 
#' Alternative assumed values include 0.1012 mL/s/mmHg for patients with Diabetes 
#' or heart failure.
#' 
#' Other caveats :
#' 1) assumes PAH elimination is nearly complete, e.g. determination of RPF by PAH
#' infusion is accurate.
#' 2) assumes the Filtation coefficient is not affected by disease. alternate values may needed 
#' for other disease such as diabetes.
#' 3) accurate only when Plasma total protein is between 5-8 g/dL, and the albumin/total protein ratio 
#' is between 0.55-0.65.
#' 4) assumes RBP is at least 1/3 of normal value (RPF > 400 mL/min))
#' 5) Some equations may be inaccurate for GFR <50 mL/min
#' 6) for Filtration Fraction > 0.33 the equations may be inaccurate
#' 7) for estimated Glomerular pressure >75 or <40, the equations may be inaccurate
#' 8) Association does not imply causation.   
#'
#' @param GFR Glomerular filtration rate measured by iohexol clearance or other
#'   precise method (mL/min), not adjusted by BSA.
#' @param RPF Renal plasma flow, determined by PAH infusion (mL/min), not adjusted by BSA.
#' @param Hct Hematocrit (fraction)
#' @param sbp Systolic Blood pressure (mmHg)
#' @param dbp Diastolic Blood pressure (mmHg)
#' @param hr Heart rate (bpm)
#' @param MAP Mean Arterial Pressure (mmHg). If not provided, MAP is calculated 
#' from `sbp`, `dbp`, and `hr`. The MAP calculation uses the formula reported by 
#' Gomez adjusting for HR, rather than the standard formula. 
#' @param DM Diabetes (`T`/`F`). defaults to `FALSE`. This changes the value used
#' for the Gross Filtration Coefficient (Kfg) to 0.1012 mL/s/mmHg.
#' @param total_protein Total protein in plasma (g/dL). Recommended default value of 7.0
#'
#' @return a Data.frame of calculated values: 
#' *  `gomez_map` Mean arterial pressure (MAP),mmHg
#' *  `gomez_ff` Filtration Fraction, unitless
#' *  `gomez_rbf` Renal Blood Flow (RBF), mL/min
#' *  `gomez_rvr` Renal Vascular Resistance (RVR), mmHg min/L
#' *  `gomez_kfg` Gross Filtration Coefficient, mL/s/mmHg
#' *  `gomez_cm` Capillary Mean Plasma protein concentration, g/dL
#' *  `gomez_pf` Filtration Pressure across Glomerular cappilaries, mmHg
#' *  `gomez_Pbow` Bowman's Space Pressure, mmHg
#' *  `gomez_piGlom` Glomerular cappilary Oncotic Pressure, mmHg
#' *  `gomez_pglom` Glomerular Pressure, mmHg
#' *  `gomez_ra`  Resistance afferent arteriole (Ra), dyne s/cm^5
#' *  `gomez_re`  Resistance efferent arteriole (Re), dyne s/cm^5
#' *  `gomez_ra_re_ratio` Ra to Re Ratio
#' *  `gomez_A` A - theoretical mean arterial pressure at which sodium excretion is zero
#' 
#' @export
#' @md
#'
#' @examples
#  # examples taken from literature using mean reported values
#' calculate_gomez(GFR=85, RPF=377, Hct=0.432,
#'                 sbp=125, dbp=73, hr=70, #MAP=93,
#'                 total_protein=7, DM=F)
#'                 
#' # Ribstein avg values
#' calculate_gomez(GFR = 102, RPF = 445, Hct = 0.44, total_protein = 7, sbp=169, dbp=97, hr=65, DM=F)
#' calculate_gomez(GFR = 93, RPF = 422, Hct = 0.44, total_protein=7, MAP=121, DM=F)
#' 
#' 
#' # Lytvyn data
#' # healthy control
#' calculate_gomez(GFR = 109, RPF = 621, Hct = 0.44, total_protein = 7, sbp=109, dbp=65, hr=61, DM=F)
#' # type I DM
#' calculate_gomez(GFR = 117, RPF = 685, Hct = 0.44, total_protein = 7, sbp=114, dbp=66, hr=70, DM=F)

calculate_gomez <- function(GFR, RPF, Hct, total_protein,
                            MAP = NULL,
                            sbp, dbp, hr,
                            DM=F){
  MAP = ifelse(is.numeric(MAP), MAP, 
               calculate_map(sbp, dbp, hr, method = "gomez"))
  GFR = GFR/60                    # convert to mL/s from mL/min
  RPF = RPF/60                    # convert to mL/s from mL/min
  
  FF = GFR/RPF                    # unitless; Filtration Fraction
  RBF = RPF/(1-Hct)               # mL/s; renal blood flow
  RVR = MAP/RBF/60*1000                # mmHg min/L
  Kfg = ifelse(!DM, 0.0867,       # mL/s/mmHg, Gross filtration coefficient (Normal)
               0.1012)            # mL/s/mmHg, Gross filtration coefficient (Diabetic)
  TP = total_protein              # g/dL; Total Protein in plasma
  CM = TP/FF * log(1/(1-FF))      # g/dL; Capillary mean plasma conc.; TP units g/dL
  dPf = GFR/Kfg                   # mmHg; Filtration pressure across glomerular capillaries
  # Gomez original formula; applies if albumin/total protein is between ~0.55 - 0.65
  # Gomez reports avg values for piG (25), Pbowman (10) and Kfg (0.0867)
  piG = ifelse(CM>=5 & CM<8, 5*(CM - 2),
               ifelse(CM>=8 & CM<11, 7.75*(CM - 4)))# mmHg; Glomerular oncotic pressure
  # commonly used form:
  # piG = 5*(CM - 2)                # mmHg; Glomerular oncotic pressure
  Pbow = 10                       # mmHg; Pbow assumed to be 10 mmHg
  Pglo = dPf + Pbow + piG         # mmHg; Pressure, glomerular capillaries

  # 1328 = conversion to dyn s/cm^5 from "mmHg s/mL"
  # standard conversion = 80 dyne s/cm^5 = mmHg min/L; also 1000/60 to convert from s/
  # 1328 converts units from mmHg*s/mL (1000/60 to min/L then * 80 to dyne s/cm^5)  
  Ra = ((MAP - Pglo)/RBF)*1328   # dyn s/cm^5; Resistance afferent arteriole; 
  Re = GFR/(Kfg * (RBF-GFR))*1328 # dyn s/cm^5; Resistance efferent arteriole; 1328 = conversion to dyn s/cm^5
  Ra_Re_ratio = Ra/Re             # no units; Ra/Re ratio
  A = Ra*RBF/1328 + Pbow + piG    # Kimura A; represents MAP at which uNa excretion stops 
  
  res = data.frame(gomez_map = MAP,
                   gomez_ff = FF,
                   gomez_rbf = RBF*60, # convert back to mL/min for reporting
                   gomez_rvr_dyne = RVR*80, # conver to dyne s 
                   gomez_rvr = RVR,
                   gomez_kfg = Kfg,
                   gomez_cm = CM,
                   gomez_pf = dPf,
                   gomez_piGlom = piG,
                   gomez_pbow = Pbow,
                   gomez_pglom = Pglo,
                   gomez_ra = Ra,
                   gomez_re = Re,
                   gomez_ra_re_ratio = Ra_Re_ratio,
                   gomez_A = A)
  return(res)
}

