#' Iohexol Single Sample (SS) estimate of GFR
#'
#' This method uses a single sample measurement of iohexol to determine GFR
#' (glomerular filtration rate), by optimizing 3 equations initially described
#' by \href{https://pubmed.ncbi.nlm.nih.gov/6684515/}{Jacobsson}. Relevant
#' Methods and recommendations are detailed in the
#' \href{https://pubmed.ncbi.nlm.nih.gov/39097002/}{European Kidney Function
#' Consortium statment paper}. The R script is modified from the supplemental methods supplied in that paper, which also supplies an excel spreadsheet which can be used for comparison. One minor difference is the use of a newer and more accurate formula to estimate ECV.
#'
#' @param egfr Model-based estimate of GFR (eg CKD-EPI eGFR). Used as a starting point for estimation.
#' @param height Height, cm  
#' @param weight Weight, kg
#' @param sex Sex (M/F)
#' @param age Age, years
#' @param iohexol_m Iohexol dose injected (mcg)
#' @param iohexol_ss Iohexol concentration (mcg/mL)
#' @param time_ss Time of iohexol sample (minutes)
#' @param weight_units Weight units, if not in kg
#' @param height_units Height units, if not in cm
#' @param ecv_method ECV Formula (default =  "Faucon"; also can use "Granerus")
#'
#' @return data.frame with `mgfr_ss` (non-indexed) and `mgfr_ss_bsa` (GFR indexed to standard BSA of 1.73m2)
#' @export calculate_mgfr_ss
#' @seealso 
#'  [calculate_ecv()] for ECV calculation
#'  [calculate_bsa()] for BSA calculation
#'
#' @examples 
#' calculate_mgfr_ss(egfr=40, 
#'                   height = 168, weight = 87, 
#'                   sex="M", age=80, 
#'                   iohexol_m = 3273820,
#'                   iohexol_ss= 60,
#'                   time_ss=390)
#'                   
#' # used alternate ECV calculation (doesn't make much difference)
#' calculate_mgfr_ss(egfr=40, 
#'                   height = 168, weight = 87, 
#'                   sex="M", age=80, ecv_method = "Granerus",
#'                   iohexol_m = 3273820,
#'                   iohexol_ss= 60,
#'                   time_ss=390)
#' 

calculate_mgfr_ss <- function(egfr, height, weight, sex, age, 
                    iohexol_m, iohexol_ss, time_ss,
                    weight_units="kg",  height_units = "cm", ecv_method="Faucon"
                    ){
  iohexol_volume = calculate_ecv(age = age, height = height, weight=weight, sex=sex,
                                 height_units = height_units, weight_units = weight_units,
                                 method=ecv_method) # mL
  bsa = calculate_bsa(weight, height, height_units = height_units, method = "DuBois")
  sol = optimize(loss_egfr, egfr,  lower=0, upper=250, 
                 iohexol_volume,
                 iohexol_m,
                 iohexol_ss,
                 time_ss) # initial guess
  return(data.frame(mgfr_ss = sol$minimum, # ml/min
                    mgfr_ss_bsa = sol$minimum/bsa*1.73)) # ml/min/1.73m2
}

# non-exported components; essential but not stand-alone functions
egfr_jacobsson <- function(egfr, 
                           iohexol_volume,
                           iohexol_m,
                           iohexol_ss,
                           time_ss) { # iohexol_volume, iohexol_m, iohexol_ss, time_ss){
  x = egfr # CL result; arbitrary start point
  m = 0.991 - (0.00122 * x) # correction-factor for non-uniform distribution
  r = 0.495 * (1 + sqrt(1 - (0.0049 * x))) # correction-factor for non-immediate mixing
  gfss = (1 / ((time_ss*m / iohexol_volume) - (log(r) / x))) * log(iohexol_m * m / (iohexol_volume*iohexol_ss)) 
  return(gfss)
}

loss_egfr <- function(egfr,...) abs(egfr_jacobsson(egfr, ...) - egfr)
