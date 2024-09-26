#' Iohexol Multiple Sample Protocol (MSP) estimate of GFR
#'
#' This method uses multiple sample measurements of iohexol to determine GFR
#' (glomerular filtration rate), using a single compartment model of the late
#' slope and adjustment using the Brochner-Mortenson correction equation.
#' Relevant Methods and recommendations are detailed in the
#' \href{https://pubmed.ncbi.nlm.nih.gov/39097002/}{European Kidney Function
#' Consortium statment paper}.
#' The function can be compared against an excel spreadsheet provided in the
#' supplemental material.
#'
#' @param time A vector of time values (minutes)
#' @param iohexol A vector of Iohexol concentrations (mcg/mL) determined at the
#'   provided times. These values should not be log transformed
#' @param omnipaque_v Omnipaque version (eg 300 = Omnipaque 300 recommended).
#'   Other versions commonly available include 300 and 350 (USA) and 140, 280,
#'   240 (other areas)
#' @param ioh_inj_wt Iohexol injection Weight by syringe weight determination
#'   (Pre-Post weight difference; recommnded method) in grams
#' @param ioh_inj_vol Iohexol injection Volume by syringe volume injected (Not
#'   preferred; use if weights not available)
#' @param height Patient Height, cm
#' @param weight Patient Weight, kg
#' @param height_units Height units, if not in cm
#' @param weight_units Weight units, if not in kg
#'
#' @return Data.frame containing results with calculated values for the
#'   following variables:
#'
#' *  `iohexol_m` Iohexol dose (mcg Iodine)
#' *  `iohexol_0` Iohexol intercept (mcg/mL)
#' *  `iohexol_k` Iohexol elimination constant (k)
#' *  `rsq` Iohexol model R-sqaured value (recommend > 0.97)
#' *  `ioh_auc` Iohexol area under the curve (AUC, mcg min / mL)
#' *  `ioh_CL` Iohexol Clearance (dose/area)
#' *  `mGFR_1c` GFR (mL/min) determined by Iohexol clearance
#' *  `mGFR_1c_bsa` GFR indexed to BSA (mL/min/1.73 m2 bsa)
#' @export calculate_mgfr_msp
#'
#' @examples
#' XLSX example data
#' df1_dem <- data.frame(height=168,     # cm
#'                       weight=87,      # kg
#'                       ioh_inj_wt = 6.82594, # injected weight g; syringe wt pre-post
#'                       ioh_vol = 5.06, # mL
#'                       ioh_conc = 647)   # Omnipaque 300
#' 
#' dat <- data.frame(time=c(160,180,200,220,232,240),
#'                   iohexol_ucg_ml = c(70,60,47,37,30,25)) 
#' 
#' calculate_mgfr_msp(dat$time, dat$iohexol_ucg_ml,
#'                    ioh_inj_vol = 5.06,
#'                    height = df1_dem$height, weight = df1_dem$weight)  
#' calculate_mgfr_msp(dat$time, dat$iohexol_ucg_ml,
#'                    ioh_inj_wt = 6.82594,
#'                    height = df1_dem$height, weight = df1_dem$weight)  
#' 
#' # Example for multiple dataset analysis
#' df2_dem <- data.frame(id=1:5,
#'                       height= rnorm(5, 168, 10),     # cm
#'                      weight= rnorm(5, 87, 5),      # kg
#'                      ioh_inj_wt = rep(6.82594,5), # injected weight g; syringe wt pre-post
#'                      ioh_vol = rep(5.06,5)) # mL
#'dat2 <- data.frame(id=rep(1:5, each=6),
#'                   time = rep(c(160,180,200,220,232,240), 5),
#'                   iohexol_ucg_ml = rep(c(70,60,47,37,30,25), 5))
#'
#'# merged with time series df in a column
#'df2_m <- merge(df2_dem, dat2, all.x = T) |> 
#'  dplyr::group_by(id) |> 
#'  tidyr::nest(dat = c(time, iohexol_ucg_ml))
#'
#'df2_m |> 
#'  mutate(mgfr = map(dat, ~calculate_mgfr_msp(.x$time, .x$iohexol_ucg_ml,
#'                                             ioh_inj_vol = ioh_vol,
#'                                             height = height, weight = weight))) |> 
#'  tidyr::unnest(mgfr)

calculate_mgfr_msp <- function(time, iohexol, 
                              omnipaque_v=300, # default
                              ioh_inj_wt=NULL, ioh_inj_vol=NULL, # must give one of these
                              height, weight, height_units="cm", weight_units="kg"){
  # Subject calcs
  bsa = calculate_bsa(weight, height, height_units = height_units, method = "DuBois")
  
  # Omnipaqe calcs; from lookup table stored internally as tabletools:::df_omnipaque
  ioh_spgrav = df_omnipaque$omnipaque_specgrav[df_omnipaque$omnipaque_v==omnipaque_v]/1000 # g/ml
  iohexol_mg_ml = df_omnipaque$iohexol_mg_ml[df_omnipaque$omnipaque_v==omnipaque_v] # mg/ml
  if (is.null(ioh_inj_wt) & is.null(ioh_inj_vol)) stop("missing injection details; must provide `ioh_inj_wt` or `ioh_inj_vol`")
  if (!is.null(ioh_inj_wt)) {ioh_inj_vol = ioh_inj_wt/ioh_spgrav  } # vol calculated by weight
  iohexol_m = ioh_inj_vol*iohexol_mg_ml*1000 # mcg
  
  # regression details
  ln_iohexol = log(iohexol) # ln of Iohexol (mcg/ml)
  lm_mod = lm(ln_iohexol ~ time)
  slope = coef(lm_mod)[["time"]]
  intercept_log = coef(lm_mod)[["(Intercept)"]]
  intercept = exp(intercept_log)
  rsq = summary(lm_mod)$r.squared
  ioh_auc = intercept/abs(slope)
  ioh_CL = iohexol_m/ioh_auc
  mGFR_1c = 0.990778*ioh_CL - 0.001218*ioh_CL^2 # Brochner-Mortenson correction
  mGFR_1c_bsa = mGFR_1c/bsa*1.73
  
  return(data.frame("iohexol_m"   = iohexol_m,
                    "iohexol_0"   = intercept,
                    "iohexol_k"   = slope,
                    "model_r2"    = rsq,
                    "ioh_auc"     = ioh_auc,
                    "ioh_CL"      = ioh_CL,
                    "mgfr_1c"     = mGFR_1c,
                    "mgfr_1c_bsa" = mGFR_1c_bsa
  ))
}
