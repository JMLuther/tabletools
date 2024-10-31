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
#' @param GFR Glomerular filtration rate measured by iohexol clearance or other
#'   precise method (mL/min), not adjusted by BSA.
#' @param RPF Renal plasma flow, determined by PAH infusion (mL/min), not adjusted by BSA.
#' @param Hct Hematocrit (fraction)
#' @param TP Total Protein concentration (g/dL, plasma)
#' @param sbp Systolic Blood pressure (mmHg)
#' @param dbp Diastolic Blood pressure (mmHg)
#' @param hr Heart rate (bpm)
#' @param DM Diabetes (`T`/`F`). defaults to `FALSE`
#'
#' @return a Data.frame of calculated values: 
#' *  `gomez_map` Mean arterial pressure (MAP),mmHg
#' *  `gomez_ff` Filtration Fraction, unitless
#' *  `gomez_rbf` Renal Blood Flow (RBF), mL/min
#' *  `gomez_rvr` Renal Vascular Resistance (RVR), mmHg min/L
#' *  `gomez_kfg` Gross Filtration Coefficient, mL/s/mmHg
#' *  `CM` Cappilary Mean Plasma protein concentration, g/dL
#' *  `gomez_pf` Filtration Pressure across Glomerular cappilaries, mmHg
#' *  `Pbow` Bowman's Space Pressure, mmHg
#' *  `gomez_piGlom` Glomerular cappilary Oncotic Pressure, mmHg
#' *  `gomez_pglom` Glomerular Pressure, mmHg
#' *  `gomez_ra`  Resistance afferent arteriole (Ra), dyne s/cm^5
#' *  `gomez_re`  Resistance efferent arteriole (Re), dyne s/cm^5
#' *  `gomez_ra_re_ratio` Ra to Re Ratio
#' *  `gomez_A` A - theoretical pressure at which sodium excretion is zero
#' 
#' @export
#' @md
#'
#' @examples
#' calculate_gomez(GFR=85, RPF=377, Hct=0.432,
#'                 sbp=125, dbp=73, hr=70, #MAP=93,
#'                 total_protein=6.8, DM=F)
#'                 
#' # Ribstein avg values
#' calculate_gomez(GFR = 102, RPF = 445, Hct = 0.44, total_protein = 6.8, sbp=169, dbp=97, hr=65, DM=F)
#' calculate_gomez(GFR = 93, RPF = 422, Hct = 0.44, total_protein=6.8, MAP=121, DM=F)
#' 
#' 
#' # Lytvyn data
#' # healthy control
#' calculate_gomez(GFR = 109, RPF = 621, Hct = 0.44, total_protein = 6.8, sbp=109, dbp=65, hr=61, DM=F)
#' # type I DM
#' calculate_gomez(GFR = 117, RPF = 685, Hct = 0.44, total_protein = 6.8, sbp=114, dbp=66, hr=70, DM=F)

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
  # Kfg = ifelse(!DM, 0.1733,       # mL/s/mmHg, Gross filtration coefficient (Normal)
  Kfg = ifelse(!DM, 0.0867,       # mL/s/mmHg, Gross filtration coefficient (Normal)
               0.1012)            # mL/s/mmHg, Gross filtration coefficient (Diabetic)
  TP = total_protein              # g/dL; Total Protein in plasma
  CM = TP/FF * log(1/(1-FF))      # g/dL; Capillary mean plasma conc.; TP units g/dL
  dPf = GFR/Kfg                   # mmHg; Filtration pressure across glomerular capillaries
  # Gomez original formula; applies if albumin/total protein is between ~0.56 - 0.64
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
                   # CM = CM,
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
