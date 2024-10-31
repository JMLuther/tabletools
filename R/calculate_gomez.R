#' Gomez formulas for renal afferent/efferent resistance
#'
#' Calculates renal resistance measures using the formulas originally reported
#' by \href{https://pubmed.ncbi.nlm.nih.gov/14888692/}{Gomez}, referred to as
#' the "Gomez Formulas". For review of this method see
#' \href{https://pubmed.ncbi.nlm.nih.gov/27605583/}{Bjornstad}. For discussion
#' of the additionally reported A value see \href{https://pubmed.ncbi.nlm.nih.gov/1860182/}{Kimura.}
#'
#' Assumptions imposed by Gomez equations are as follows: 
#' 1) intrarenal vascular resistances are divided into afferent, postglomerular, and efferent
#' 2)hydrostatic pressures within the renal tubules, venules, Bowman’s space, and interstitium (PBow) are in equilibrium of 10 mmHg; 
#' 3) glomerulus is  in filtration disequilibrium
#' 4) the gross filtration coefficient KF) is 0.0867mL/s/ mmHg given a normal kidney.
#'
#' @param GFR Glomerular filtration rate measured by iohexol clearance or other
#'   precise method (mL/min)
#' @param RPF Renal plasms flow, determined by PAH infusion (mL/min)
#' @param Hct Hematocrit (fraction)
#' @param TP Total protein concentration (mg/dL, plasma)
#' @param sbp Systolic Blood pressure (mmHg)
#' @param dbp Diastolic Blood pressure (mmHg)
#' @param hr Heart rate (bpm)
#' @param DM Diabetes (`T`/`F`). defaults to `FALSE`
#'
#' @return Data.frame of calculated values
#' @export
#' @md
#'
#' @examples
#' calculate_gomez(GFR=85/60, RPF=377/60, Hct=0.432,
#'                 sbp=125, dbp=73, hr=70, #MAP=93,
#'                 TP=6.8, DM=F)

calculate_gomez <- function(GFR, RPF, Hct, TP,
                            sbp, dbp, hr,
                            DM=F){
  MAP = calculate_map(sbp, dbp, hr, method = "gomez")
  FF = GFR/RPF                    # unitless; Filtration Fraction
  RBF = RPF/(1-Hct)               # mL/s; renal blood flow
  Kfg = ifelse(!DM, 0.0867,       # mL/s/mmHg, Gross filtration coefficient (Normal)
               0.1012)            # mL/s/mmHg, Gross filtration coefficient (Diabetic)
  # Kfg # Ultrafiltration coefficient
  
  
  CM = TP/FF * log(1/(1-FF))      # ?; Capillary mean plasma conc.; TP units mg/dL
  dPf = GFR/Kfg                   # mmHg; Filtration pressure across glomerular capillaries
  piG = 5*(CM - 2)                # mmHg; Glomerular oncotic pressure
  Pbow = 10                       # mmHg; Pbow assumed to be 10 mmHg
  Pglo = dPf + Pbow + piG         # mmHg; Pressure, glomerular capillaries
  
  Ra = ((MAP - Pglo)/RBF)*1328   # dyn s/cm^5; Resistance afferent arteriole; 1328 = conversion to dyn s/cm^5
  Re = (GFR/Kfg * (RBF-GFR))*1328 # dyn s/cm^5; Resistance efferent arteriole; 1328 = conversion to dyn s/cm^5
  Ra_Re_ratio = Ra/Re             # no units; Ra/Re ratio
  A = Ra*RBF/1328 + Pbow + piG    # Kimura A; represents MAP at which uNa excretion stops 
  
  res = data.frame(gomez_map = MAP,
                   gomez_ff = FF,
                   gomez_rbf = RBF,
                   gomez_kfg = Kfg,
                   CM = CM,
                   dPf = dPf,
                   Pbow = Pbow,
                   gomez_pglom = Pglo,
                   gomez_ra = Ra,
                   gomez_re = Re,
                   gomez_ra_re_ratio = Ra_Re_ratio,
                   gomez_A = A)
  return(res)
}
