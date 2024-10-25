#' Iohexol Multiple Sample Protocol (MSP) estimate of GFR
#'
#' This method uses multiple sample measurements of iohexol to determine GFR
#' (glomerular filtration rate), using a single compartment model of the late
#' slope and adjustment using the Brochner-Mortenson correction equation (.
#' Relevant Methods and recommendations are detailed in the
#' \href{https://pubmed.ncbi.nlm.nih.gov/39097002/}{European Kidney Function
#' Consortium statment paper}.
#' The function can be compared against an excel spreadsheet provided in the
#' supplemental material.
#' 
#' @inheritParams calculate_mgfr_2c
#' @md
#' @export
#' @examples
#' library(tabletools)
#' library(dplyr)
#' # data from Ebert KI 2024, in online XLS file: https://pubmed.ncbi.nlm.nih.gov/39097002/
#' df1_dem <- data.frame(height=168,     # cm
#'                       weight=87,      # kg
#'                       ioh_inj_wt = 6.82594, # injected weight g; syringe wt pre-post
#'                       ioh_vol = 5.06, # mL
#'                       ioh_conc = 647)   # Omnipaque 300
#' 
#' dat_ebert
#' 
#' calculate_mgfr_msp(dat_ebert$time, dat_ebert$iohexol,
#'                    ioh_inj_vol = 5.06,
#'                    height = df1_dem$height, weight = df1_dem$weight)  
#' calculate_mgfr_msp(dat_ebert$time, dat_ebert$iohexol,
#'                    ioh_inj_wt = 6.82594, 
#'                    ioh_inj_vol=NULL, # make this NULL if weight given
#'                    height = df1_dem$height, weight = df1_dem$weight)  
#'                    
#' # methods designed to match the fuller for 2C
#' calculate_mgfr_msp(dat_ebert$time, dat_ebert$iohexol, height = 1.68, weight=87, ioh_inj_vol = 5.06, output = "summary")
#' calculate_mgfr_msp(dat_ebert$time, dat_ebert$iohexol, height = 1.68, weight=87, ioh_inj_vol = 5.06, output = "gfr")
#' calculate_mgfr_msp(dat_ebert$time, dat_ebert$iohexol, height = 1.68, weight=87, ioh_inj_vol = 5.06, output = "gfr_bsa")
#' calculate_mgfr_msp(dat_ebert$time, dat_ebert$iohexol, height = 1.68, weight=87, ioh_inj_vol = 5.06, output = "fit")
#' calculate_mgfr_msp(dat_ebert$time, dat_ebert$iohexol, height = 1.68, weight=87, ioh_inj_vol = 5.06, output = "plot", id="test")
#' calculate_mgfr_msp(dat_ebert$time, dat_ebert$iohexol, height = 1.68, weight=87, 
#' ioh_inj_vol = 5.06, output = "plot", id="test", legend_cex = 1.5)
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
#'library(purrr)
#'df2_m |> 
#'  mutate(mgfr = map(dat, ~calculate_mgfr_msp(.x$time, .x$iohexol_ucg_ml,
#'                                             ioh_inj_vol = ioh_vol,
#'                                             height = height, weight = weight))) |> 
#'  tidyr::unnest(mgfr)

calculate_mgfr_msp <- function(time, iohexol_conc, 
                               omnipaque_v=300, ioh_inj_vol=5.0,
                               ioh_inj_wt=NULL,  
                               ioh_units="ug/mL", time_units="min",
                               id=NULL,
                               height=NA, height_units = "m", 
                               weight=NA, weight_units = "kg",
                               method_adj = "BM", t_late = 120, #t_early = 100,
                               legend_cex = 1.0,
                               output="summary"){
  # Subject calcs
  bsa = ifelse(is.na(height) || is.na(weight), NA, 
               calculate_bsa(weight, height, height_units = height_units, method = "DuBois"))
  
  # Omnipaqe calcs; from lookup table stored internally as tabletools:::df_omnipaque
  # Note: standard 5mL injection of Iohexol iohexol_v = 5 # mL = 3235 mg Iohexol.
  # df_omnipaque=tabletools:::df_omnipaque # available in internal data
  ioh_spgrav = df_omnipaque$omnipaque_specgrav[df_omnipaque$omnipaque_v==omnipaque_v]/1000 # g/ml
  iohexol_mg_ml = df_omnipaque$iohexol_mg_ml[df_omnipaque$omnipaque_v==omnipaque_v] # mg/ml
  if (!is.null(ioh_inj_wt) & !is.null(ioh_inj_vol)) {
    ioh_inj_vol = ioh_inj_wt/ioh_spgrav
    rlang::warn("Iohexol injection weight is provided; `ioh_inj_vol` recalculated.")}
  if (is.null(ioh_inj_wt) & is.null(ioh_inj_vol)) stop("missing injection details; must provide `ioh_inj_wt` or `ioh_inj_vol`")
  if (!is.null(ioh_inj_wt)) {ioh_inj_vol = ioh_inj_wt/ioh_spgrav  } # vol calculated by weight
  iohexol_m = ioh_inj_vol*iohexol_mg_ml*1000 # mass mcg iohexol injected
  
  # Data: get time, iohexol data
  time_min =  convert_time_to_min(time, time_units)
  iohexol_valid = ifelse(tolower(ioh_units) %in% c("ug/ml","mcg/ml","mg/l"), T, NA)
  iohexol = iohexol_conc[iohexol_valid]
  
  # get initial guess from SI method
  dat = data.frame(time=time_min, iohexol=iohexol)
  # wt <- if (nls_weights) {1/iohexol^2} else {rep(1, length(dat$iohexol))} # weights for NLLS
  
  # Late Timepoints 
  # t_late = 120
  dat <- dat[dat$time >= t_late, ]
  lm_late <- lm(log(iohexol) ~ time, data = dat)
  B <-  exp(coef(lm_late))[["(Intercept)"]]
  b <- -coef(lm_late)[["time"]]
  
  AUC_inf <- B/b  # AUC_inf 
  mgfr_late = iohexol_m/AUC_inf  # Dose/AUC_inf = mL/minutes (not indexed to BSA)
  # Adjust for unmeasured early Iohexol conc (standard Brochner-Mortenson correction)
  # can add other options for alternative adjustments
    if (method_adj == "BM") {
      mgfr_msp = 0.990778*mgfr_late - 0.001218*mgfr_late^2}
  mgfr_msp_bsa = mgfr_msp/bsa*1.73
  mgfr_method = paste0("MSP-", method_adj)
  
  # mgfr_2c_bsa = ifelse(!is.na(bsa), mgfr_2c*1.73/bsa, NA)  # indexed to BSA
  iohexol_vd = NA # iohexol_m/1000/(A+B) # volume of distribution, L
  
  # model fit parameters
  # fit_SI_vals_late <- function(t, A, a, B, b) {A*exp(-a*t)+B*exp(-b*t) }
  dat$pred <- exp(predict(lm_late))
  dat$resid <- exp(residuals(lm_late)) #  transform to reg scale
  # ODE micro parameters:
  k10 = NA # iohexol_m/iohexol_vd/AUC_inf/1000 # 1/min
  k21 = NA #a*b/k10
  k12 = NA #a+b - k10 - k21
  
  model_r2 =  summary(lm(pred ~ iohexol, data = dat))[["r.squared"]]  # this is a pseudo-r2
  ssr = NA #sum(dat$resid^2)
  sse = NA #sum(dat$resid[dat$time <= t_early]^2)
  ssl = sum(dat$resid^2)
  res = data.frame("mgfr_method" = mgfr_method,
                   "mgfr_2c"     = mgfr_msp,
                   "mgfr_2c_bsa" = mgfr_msp_bsa,
                   "iohexol_m"   = iohexol_m,
                   "iohexol_0"   = NA, # A+B,
                   "iohexol_vd"  = iohexol_vd,
                   "ioh_auc"     = AUC_inf,
                   "A"           = NA, #A,
                   "a"           = NA, #a,
                   "B"           = B,
                   "b"           = b,
                   "model_r2"    = model_r2, # this is a pseudo-r2
                   "ssr"         = ssr,
                   "sse"         = sse,
                   "ssl"         = ssl,
                   "k10"         = k10,
                   "k21"         = k21,
                   "k12"         = k12,
                   "n_early"     = NA, #length(dat_early$time),
                   "n_late"     = length(dat$time))
  if (output=="summary") {  return(res)}
  if (output == "gfr") return(mgfr_msp) # mGFR adjusted to 1.73m2 BSA
  if (output == "gfr_bsa") return(mgfr_msp_bsa) # mGFR adjusted to 1.73m2 BSA
  if (output == "fit") {return(lm_late)}
  if (output == "plot") {
    plot_msp_fit <- function(model, time, iohexol) {
      # Main plot:
      time_range = min(time_min):max(time_min)
      plot(time, iohexol, pch=16, xlab = "Time after injection (minutes)", ylab="Iohexol (ug/mL)",
           xlim = c(0,max(time)*1.05), ylim = c(0, max(iohexol)*1.5),
           cex.lab=legend_cex, cex.axis=legend_cex, cex.main=legend_cex, cex.sub=legend_cex)
      lines(time_range, exp(predict(lm_late, newdata = data.frame(time=min(time_min):max(time_min)))), col="red", lty=2)
      title(main = ifelse(!is.null(id),      # add subtitle with subject identifier
                          paste0("Study: ",id, "\nIohexol measured GFR, Multiple Sample 1-Compartment model\nMethod: ", mgfr_method), 
                          paste0("Iohexol measured GFR, Multiple Sample 1-Compartment model\nMethod: ", mgfr_method)), 
            adj = 0, cex.main=legend_cex)
      # Build Legend - model summary values:
      A  = NA
      a  = NA
      B  = B |> round(1)
      b  = b |> round(4)
      mgfr_msp = mgfr_msp |> round(1)
      mgfr_msp_bsa = mgfr_msp_bsa |> round(1)
      AUC_inf = AUC_inf |> round(1)
      model_r2 = model_r2 |> round(4)
      iohexol_vd = iohexol_vd |> round(2)
      legend("topleft", adj=0.02, cex = legend_cex,
             legend=c(bquote(GFR==.(mgfr_msp)~mL/min),
                      bquote(GFR[bsa-adj]==.(mgfr_msp_bsa)~mL/min/1.73~m^2),
                      bquote(AUC[inf]==.(AUC_inf)~ug/mL*min),
                      bquote(pseudo-R^2==.(model_r2))) )
    }
    return(plot_msp_fit(lm_late, time_min, iohexol))
  }
  
  }

