#' Iohexol measured GFR with 2-compartment model (early and late sampling)
#'
#' This method requires multiple early (alpha phase, typically <120 min) and
#' late (beta phase) sample measurements of iohexol to determine GFR (glomerular
#' filtration rate). This function uses nonlinear modeling via the `nls()`
#' function to fit the 2-compartment model of iohexol kinetics using the formula \eqn{C=A \cdot exp^{- \alpha \cdot t} +
#' B*e^{- \beta \cdot t} } and weighted by \eqn{1/Iohexol^2}. The general method and provided examples are
#' modified from \href{https://pubmed.ncbi.nlm.nih.gov/16612328/}{Schwartz et al. 
#' Kidney Int. 2006.}. Further discussion and details regarding methods of fitting the 2-compartment models can be found at \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8101203/}{Pottel et al.} Results are returned as a non-indexed value for GFR (`mgfr_2c`) and also indexed to BSA of 1.73m2 if height and weight are provided (`mgfr_2c_bsa`).
#' The summary provides estimates of kinetic parameters (k10, k21, k12) which
#' can be used in ODE models, but is not implemented here.
#'
#' @param time A vector of time values (minutes)
#' @param iohexol_conc A vector of Iohexol plasma measurements (ug/mL)
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
#' @param ioh_units Iohexol concentration units, defaults to `ug/mL`
#' @param id Study identifier (optional, passed to plot title)
#' @param time_units Time units, defaults to `min`
#' @param nls_weights Use weights in nls model, defaults to `TRUE`. Uses
#'   1/iohexol^2 as weights, which more heavily weight lower concentrations
#'   obtained at later time points
#' @param output Desired output, defaults to `summary` of model. Alternatively
#'   can specify `gfr`, `gfr_bsa`, `fit`, or `plot`
#'
#'   `summary` results include the named variables:
#'    * `mgfr_2c` The measured GFR (mL/min)
#'    * `mgfr_2c_bsa` The measured GFR (mL/min/1.73 m2) indexed to body surface area (BSA calculated by DuBois equation)
#'    * `iohexol_m`   Iohexol mass (ucg) injected. Determined either by user-provided syringe weight or volume injected.
#'    * `iohexol_0`   Iohexol concentration at t=0 calculated by A+B (theoretical, not measured)
#'    * `iohexol_vd`  Iohexol Volume of distribution estimated by Dose/Iohexol[t0]
#'    * `ioh_auc`  Iohexol calculated AUC from t=0 to Infinity, estimated by A/a + B/b
#'    * `A` Model parameter A
#'    * `a` Model parameter a, or \alpha
#'    * `B` Model parameter B
#'    * `b` Model parameter b, or \beta
#'    * `model_r2` Model Pseudo-R2, calculated as the linear regression R2 for predicted~observed values
#'    * `k10` Iohexol elimination rate constant from central compartment (1/min)
#'    * `k21` Iohexol transfer rate constant from peripheral to central compartment (1/min)
#'    * `k12` Iohexol transfer rate constant from central to peripheral compartment (1/min)
#'
#'   `gfr` returns a single `mgfr_2c` value, the measured GFR (mL/min).
#'   `gfr_bsa` returns a single `mgfr_2c_bsa` value, the measured GFR
#'   (mL/min/1.73 m2) indexed to body surface area (BSA calculated by DuBois
#'   equation).
#'
#'   `fit` returns the non-linear regression model fit object.
#'
#'   `plot` returns a base-R plot of observed and predicted regression curve vs
#'   time, and summary measures in the legend.
#'
#' @return Desired output with either a data.frame of results (`summary`), a
#'   single value of BSA adjusted GFR (`gfr`), an `nls object` for model fit
#'   (`fit`), or a plot with observed values, model fit curve, and summary
#'   results in the figure legend (`plot`)
#' @export
#' @md
#'
#' @examples
#' ## ├ Scwartz Data ----
#' # Data extracted from Schwartz Fig1 PMID: 16612328
#' # Iohexol 5mL IV injection (Omnipaque 300, 5mL ~3235mg Iohexol)
#' # sampling at 10, 20, 30, 60, 120, 240, 300, 360 min
#' # time (minutes)
#' # Iohexol (ug/ml)
#' # age, height, weight not known for the example
#' dat <-
#'   data.frame(
#'     time = c(10, 20, 30, 60, 120, 180, 240, 300, 360),
#'     iohexol_ug_ml = c(656.1168132,477.1163595,371.3542728,
#'                       223.1121251,111.1086272,61.88251616,
#'                       37.43137242,21.79250307,12.75996292)  )
#'
#' calculate_mgfr_2c(dat$time, dat$iohexol_ug_ml, height = 1.67, weight = 70, ioh_inj_vol = 5)
#' calculate_mgfr_2c(dat$time, dat$iohexol_ug_ml, height = 1.67, weight = 70, ioh_inj_vol = 5, output="gfr")
#' calculate_mgfr_2c(dat$time, dat$iohexol_ug_ml, height = 1.67, weight = 70, ioh_inj_vol = 5, output="gfr_bsa")
#' calculate_mgfr_2c(dat$time, dat$iohexol_ug_ml, height = 1.67, weight = 70, ioh_inj_vol = 5, output="fit")
#' calculate_mgfr_2c(dat$time, dat$iohexol_ug_ml, height = 1.67, weight = 70, ioh_inj_vol = 5, output="plot")
#' calculate_mgfr_2c(dat$time, dat$iohexol_ug_ml, height = 1.67, weight = 70, ioh_inj_vol = 5, output="plot", nls_weights = FALSE)
#' calculate_mgfr_2c(dat$time, dat$iohexol_ug_ml, height = 1.67, weight = 70, ioh_inj_vol = 5, output="plot", id="Name-IDnumber-Date")
#'
#' # examples with fewer time points
#' dat_5p <- dat[dat$time %in% c(10, 20, 30, 120, 300), ]
#' calculate_mgfr_2c(dat_5p$time, dat_5p$iohexol_ug_ml, height = 1.67, weight = 70, ioh_inj_vol = 5, output="plot")
#'
#' dat_5p <- dat[dat$time %in% c(10, 30, 60, 120, 300), ]
#' calculate_mgfr_2c(dat_5p$time, dat_5p$iohexol_ug_ml, height = 1.67, weight = 70, ioh_inj_vol = 5, output="plot")
#'
#' dat_7p <- dat[dat$time %in% c(10, 20, 30, 60, 240, 300, 360), ]
#' calculate_mgfr_2c(dat_7p$time, dat_7p$iohexol_ug_ml, height = 1.67, weight = 70, ioh_inj_vol = 5, output="plot")
#'
#' # ODE MODEL FOR COMPARISON;
#' # Example only to verify ODE parameter estimates, not required for GFR calculation
#' # This model uses solution from the nls model to get the kinetic parameters
#' library(deSolve)
#' m2c <- function(Time, State, Pars) {
#'   with(as.list(c(State, Pars)), {
#'     dIohexol <- -k10*Iohexol - k12*Iohexol + k21*C2 # Central Conc
#'     dC2 <-  k12*Iohexol - k21*C2                    # Peripheral Conc
#'     return(list(c(dIohexol, dC2)))
#'   })
#' }
#' # get parameters from nls model
#' ioh_nlfit <- calculate_mgfr_2c(dat$time, dat$iohexol_ug_ml, height = 1.67, weight = 70, ioh_inj_vol = 5)
#' pars  <- c(k10 = ioh_nlfit$k10, k21 = ioh_nlfit$k21, k12 = ioh_nlfit$k12)
#' # plug into the ODE model solution
#' yini  <- c(Iohexol = ioh_nlfit$A+ioh_nlfit$B, C2=0)
#' times <- seq(0, 360, by = 1)
#' out   <- ode(yini, times, m2c, pars)
#' out_df <- as.data.frame(out)
#' # Plot to verfiy kinetic parameters vs nls model
#' calculate_mgfr_2c(dat$time, dat$iohexol_ug_ml, height = 1.67, weight = 70, ioh_inj_vol = 5, output="plot")
#' lines(out_df$time, out_df$Iohexol, lty=2)


calculate_mgfr_2c <- function(time, iohexol_conc, 
                              omnipaque_v=300, ioh_inj_vol=5.0,
                              ioh_inj_wt=NULL,  
                              ioh_units="ug/mL", time_units="min",
                              id=NULL,
                              height=NA, height_units = "m", 
                              weight=NA, weight_units = "kg",
                              nls_weights = TRUE,
                              output="summary"
){
  # Subject calcs
  bsa = ifelse(is.na(height) || is.na(weight), NA, 
               calculate_bsa(weight, height, height_units = height_units, method = "DuBois"))
  
  # Omnipaqe calcs; from lookup table stored internally as tabletools:::df_omnipaque
  # Note: standard 5mL injection of Iohexol iohexol_v = 5 # mL = 3235 mg Iohexol.
  ioh_spgrav = df_omnipaque$omnipaque_specgrav[df_omnipaque$omnipaque_v==omnipaque_v]/1000 # g/ml
  iohexol_mg_ml = df_omnipaque$iohexol_mg_ml[df_omnipaque$omnipaque_v==omnipaque_v] # mg/ml
  if (!is.null(ioh_inj_wt) & !is.null(ioh_inj_vol)) stop("If iohexol injection weight is provided, assign `ioh_inj_vol=NULL` and exact iohexol mass will be calculated.")
  if (is.null(ioh_inj_wt) & is.null(ioh_inj_vol)) stop("missing injection details; must provide `ioh_inj_wt` or `ioh_inj_vol`")
  if (!is.null(ioh_inj_wt)) {ioh_inj_vol = ioh_inj_wt/ioh_spgrav  } # vol calculated by weight
  iohexol_m = ioh_inj_vol*iohexol_mg_ml*1000 # mass mcg iohexol injected
  
  # Data: get time, iohexol data
  time_min =  convert_time_to_min(time, time_units)
  iohexol_valid = ifelse(tolower(ioh_units) %in% c("ug/ml","mcg/ml","mg/l"), T, NA)
  iohexol = iohexol_conc[iohexol_valid]
  wt <- 1/iohexol^2
  A_start = iohexol[1]*0.7
  B_start = iohexol[1]*0.3
  
  # fit model to data
  nonlin2c <- function(t, A, a, B, b) { A*(exp(-a*t)) + B*(exp(-b*t))   }
  fit = nls(iohexol  ~ nonlin2c(time_min, A, a, B, b),
            data=data.frame(time_min, iohexol, wt),
            start=list(A = A_start,
                       B = B_start,
                       a =0.03,
                       b =0.004),
            weights = if (nls_weights) wt else NULL)
  A  = coef(fit)[["A"]]
  a  = coef(fit)[["a"]]
  B  = coef(fit)[["B"]]
  b  = coef(fit)[["b"]]
  model_r2 = summary(lm(iohexol ~ predict(fit)))[["r.squared"]]   # this is a pseudo-r2
  AUC_inf <- A/a + B/b  # AUC_inf 
  mgfr_2c = iohexol_m/AUC_inf  # Dose/AUC_inf = mL/minutes (not indexed to BSA)
  mgfr_2c_bsa = ifelse(!is.na(bsa), mgfr_2c*1.73/bsa, NA)  # indexed to BSA
  iohexol_vd = iohexol_m/1000/(A+B) # volume of distribution, L
  # ODE micro parameters:
  k10 = iohexol_m/iohexol_vd/AUC_inf/1000 # 1/min
  k21 = a*b/k10
  k12 = a+b - k10 - k21
  
  if (output == "summary") {
    res = data.frame("mgfr_2c"     = mgfr_2c,
                     "mgfr_2c_bsa" = mgfr_2c_bsa,
                     "iohexol_m"   = iohexol_m,
                     "iohexol_0"   =  A+B,
                     "iohexol_vd"  = iohexol_vd,
                     "ioh_auc"     = AUC_inf,
                     "A"           = A,
                     "a"           = a,
                     "B"           = B,
                     "b"           = b,
                     "model_r2"    = model_r2, # this is a pseudo-r2
                     "k10"         = k10,
                     "k21"         = k21,
                     "k12"         = k12) 
    return(res)}
  if (output == "gfr") return(mgfr_2c) # mGFR adjusted to 1.73m2 BSA
  if (output == "gfr_bsa") return(mgfr_2c_bsa) # mGFR adjusted to 1.73m2 BSA
  if (output == "fit") return(fit)
  if (output == "plot") {
    plot_nls_fit <- function(model, time, iohexol) {
      # Main plot:
      time_range = 0:max(time)
      plot(time, iohexol, pch=16, xlab = "Time after injection (minutes)", ylab="Iohexol (ug/mL)")
      lines(predict(model, list(time_min=0:max(time))), col="red", lty=2)
      title(main = ifelse(!is.null(id),      # add subtitle with subject identifier
                          paste0("Study: ",id, "\nIohexol measured GFR, 2-Compartment model"), 
                          "Iohexol measured GFR, 2-Compartment model"), adj = 0)
      # model summary values:
      A  = coef(model)[["A"]] |> round(1)
      a  = coef(model)[["a"]] |> round(4)
      B  = coef(model)[["B"]] |> round(1)
      b  = coef(model)[["b"]] |> round(4)
      mgfr_2c = mgfr_2c |> round(1)
      mgfr_2c_bsa = mgfr_2c_bsa |> round(1)
      AUC_inf = AUC_inf |> round(1)
      model_r2 = model_r2 |> round(4)
      iohexol_vd = iohexol_vd |> round(2)
      legend("topright", adj=0.02, cex = 0.9,
             legend=c(bquote(C[Ioh.]==.(A)*e^(-.(a)*t)+.(B)*e^(-.(b)*t)),
                      bquote(GFR==.(mgfr_2c)~mL/min),
                      bquote(GFR[bsa-adj]==.(mgfr_2c_bsa)~mL/min/1.73~m^2),
                      bquote(AUC[inf]==.(AUC_inf)~ug/mL*min),
                      bquote(Vd==.(iohexol_vd)~L),
                      bquote(pseudo-R^2==.(model_r2))) )
    }
    plot_nls_fit(fit, time_min, iohexol)}
}
