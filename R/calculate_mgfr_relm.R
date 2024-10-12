#' Relmipirazin measured GFR with 2-compartment model (early and late sampling)
#'
#' This method requires multiple early (alpha phase, typically <120 min) and
#' late (beta phase) sample measurements of relmipirazin to determine GFR (glomerular
#' filtration rate). This function uses nonlinear modeling via the `nls()`
#' function to fit the 2-compartment model of relmipirazin kinetics using the formula \eqn{C=A*e^(- \alpha * t) +
#' B*e^(- \beta * t) }. The general method and provided examples are
#' modified from \href{https://pubmed.ncbi.nlm.nih.gov/16612328/}{Schwartz et al.
#' Kidney Int. 2006.}. Results are returned as a non-indexed value for GFR (`mgfr_2c`) and also indexed to BSA of 1.73m2 if height and weight are provided (`mgfr_2c_bsa`).
#' The summary provides estimates of kinetic parameters (k10, k21, k12) which can be used in ODE models, but is not implemented here. 
#'
#' @param time A vector of time values (minutes)
#' @param relmipirazin_conc A vector of relmipirazin plasma measurements (ug/mL)
#' @param relm_inj_wt Relmipirazin injection Weight by syringe weight determination
#'   (Pre-Post weight difference; recommended method) in grams
#' @param relm_inj_vol Relmipirazin injection Volume by syringe volume injected (Not
#'   preferred; use if weights not available)
#' @param height Patient Height, cm
#' @param weight Patient Weight, kg
#' @param height_units Height units, if not in cm
#' @param weight_units Weight units, if not in kg
#' @param relm_units relmipirazin concentration units, defaults to `ng/mL`
#' @param id Study identifier (optional, passed to plot title)
#' @param time_units Time units, defaults to `min`
#' @param output Desired output, defaults to `summary` of model. Alternatively
#'   can specify `gfr`, `gfr_bsa`, `fit`, or `plot`
#'
#'   `summary` results include the named variables:
#'    * `mgfr_2c` The measured GFR (mL/min)
#'    * `mgfr_2c_bsa` The measured GFR (mL/min/1.73 m2) indexed to body surface area (BSA calculated by DuBois equation)
#'    * `relmipirazin_m`   Relmipirazin mass (ucg) injected. Determined either by user-provided syringe weight or volume injected.
#'    * `relmipirazin_0`   Relmipirazin concentration at t=0 calculated by A+B (theoretical, not measured)
#'    * `relmipirazin_vd`  Relmipirazin Volume of distribution estimated by Dose/relmipirazin[t0]
#'    * `relm_auc`  Relmipirazin calculated AUC from t=0 to Infinity, estimated by A/a + B/b
#'    * `A` Model parameter A
#'    * `a` Model parameter a, or \alpha
#'    * `B` Model parameter B
#'    * `b` Model parameter b, or \beta
#'    * `model_r2` Model Pseudo-R2, calculated as the linear regression R2 for predicted~observed values
#'    * `k10` Relmipirazin elimination rate constant from central compartment (1/min)
#'    * `k21` Relmipirazin transfer rate constant from peripheral to central compartment (1/min)
#'    * `k12` Relmipirazin transfer rate constant from central to peripheral compartment (1/min)
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
#' 
#' # ODE MODEL FOR COMPARISON; 
#' # Example only to verify ODE parameter estimates, not required for GFR calculation
#' # This model uses solution from the nls model to get the kinetic parameters
#' library(deSolve)
#' m2c <- function(Time, State, Pars) {
#'   with(as.list(c(State, Pars)), {
#'     drelmipirazin <- -k10*relmipirazin - k12*relmipirazin + k21*C2 # Central Conc
#'     dC2 <-  k12*relmipirazin - k21*C2                    # Peripheral Conc
#'     return(list(c(drelmipirazin, dC2)))
#'   })
#' }
#' # get parameters from nls model
#' relm_nlfit <- calculate_mgfr_2c(dat$time, dat$relmipirazin_ug_ml, height = 1.67, weight = 70, relm_inj_vol = 5)
#' pars  <- c(k10 = relm_nlfit$k10, k21 = relm_nlfit$k21, k12 = relm_nlfit$k12)
#' # plug into the ODE model solution
#' yini  <- c(relmipirazin = relm_nlfit$A+relm_nlfit$B, C2=0)
#' times <- seq(0, 360, by = 1)
#' out   <- ode(yini, times, m2c, pars)
#' out_df <- as.data.frame(out)
#' # Plot to verfiy kinetic parameters vs nls model
#' calculate_mgfr_2c(dat$time, dat$relmipirazin_ug_ml, height = 1.67, weight = 70, relm_inj_vol = 5, output="plot")
#' lines(out_df$time, out_df$relmipirazin, lty=2)

calculate_mgfr_relm <- function(time, relmipirazin_conc, 
                              # omnipaque_v=300, 
                              # relm_inj_vol=5.0,
                              # relm_inj_wt=NULL,
                              relm_units="ng/mL", time_units="min",
                              id=NULL,
                              height=NA, height_units = "m", 
                              weight=NA, weight_units = "kg",
                              nls_weights = TRUE,
                              output="summary"
){
  # Subject calcs
  weight = tabletools::convert_weight_to_kg(weight, weight_units)
  bsa = ifelse(is.na(height) || is.na(weight), NA, 
               calculate_bsa(weight, height, height_units = height_units, method = "DuBois"))
  # if (!is.null(relm_inj_wt) & !is.null(relm_inj_vol)) stop("If relmipirazin injection weight is provided, assign `relm_inj_vol=NULL` and exact relmipirazin mass will be calculated.")
  # if (is.null(relm_inj_wt) & is.null(relm_inj_vol)) stop("missing injection details; must provide `relm_inj_wt` or `relm_inj_vol`")
  # relmipirazin_m = relm_inj_vol*relmipirazin_mg_ml*1000 # mass mcg relmipirazin injected
  relmiparazin_m = 1.5*weight * 1000 # ucg
  
  # Data: get time, relmipirazin data
  time_min =  convert_time_to_min(time, time_units)
  relmipirazin_valid = ifelse(tolower(relm_units) %in% c("ng/ml", "mcg/L", "ug/L"), T, NA)
  relmipirazin = relmipirazin_conc[relmipirazin_valid] 
  wt <- 1/relmipirazin^2
  A_start = relmipirazin[1]*0.7
  B_start = relmipirazin[1]*0.3
  
  # fit model to data
  nonlin2c <- function(t, A, a, B, b) { A*(exp(-a*t)) + B*(exp(-b*t))   }
  fit = nls(relmipirazin  ~ nonlin2c(time_min, A, a, B, b),
            data=data.frame(time_min, relmipirazin, wt),
            start=list(A = A_start,
                       B = B_start,
                       a =0.03,
                       b =0.004),
            weights = if(nls_weights) wt else NULL)
  A  = coef(fit)[["A"]]
  a  = coef(fit)[["a"]]
  B  = coef(fit)[["B"]]
  b  = coef(fit)[["b"]]
  model_r2 = summary(lm(relmipirazin ~ predict(fit)))[["r.squared"]]   # this is a pseudo-r2
  AUC_inf <- A/a + B/b  # AUC_inf (ng*min/ml)) # ug*min/L
  mgfr_2c = relmipirazin_m/AUC_inf *1000   # Dose/AUC_inf = ucg / (ucg*min/L)*10^3mL/L; mL/min (not indexed to BSA)
  mgfr_2c_bsa = ifelse(!is.na(bsa), mgfr_2c*1.73/bsa, NA)  # indexed to BSA
  relmipirazin_vd = relmipirazin_m/(A+B) # volume of distribution, L
  # ODE micro parameters:
  k10 = relmipirazin_m/relmipirazin_vd/AUC_inf/1000 # 1/min
  k21 = a*b/k10
  k12 = a+b - k10 - k21
  
  if (output == "summary") {
    res = data.frame("mgfr_2c_relm"     = mgfr_2c,        # mL/min
                     "mgfr_2c_relm_bsa" = mgfr_2c_bsa,    # mL/min/1.73m2
                     "relmipirazin_m"   = relmipirazin_m, # ucg
                     "relmipirazin_0"   =  A+B,           # ng/mL
                     "relmipirazin_vd"  = relmipirazin_vd,# L
                     "relm_auc"     = AUC_inf,            # ng*min/mL or ucg*min/L
                     "A"           = A,
                     "a"           = a,
                     "B"           = B,
                     "b"           = b,
                     "model_r2"    = model_r2, # this is a pseudo-r2
                     "k10"         = k10,      # 1/min
                     "k21"         = k21,      # 1/min
                     "k12"         = k12)      # 1/min
    return(res)}
  if (output == "gfr") return(mgfr_2c) # mGFR adjusted to 1.73m2 BSA
  if (output == "gfr_bsa") return(mgfr_2c_bsa) # mGFR adjusted to 1.73m2 BSA
  if (output == "fit") return(fit)
  if (output == "plot") {
    plot_nls_fit <- function(model, time, relmipirazin) {
      # Main plot:
      time_range = 0:max(time)
      plot(time, relmipirazin, pch=21, xlab = "Time after injection (minutes)", ylab="Relmipirazin (ng/mL)",
           log="y", col="red")
      lines(predict(model, list(time_min=0:max(time))), col="blue", lty=2)
      title(main = ifelse(!is.null(id),      # add subtitle with subject identifier
                          paste0("Study: ",id, "\nRelmipirazin measured GFR, 2-Compartment model"), 
                          "Relmipirazin measured GFR, 2-Compartment model"), adj = 0)
      # model summary values:
      A  = coef(model)[["A"]] |> round(1)
      a  = coef(model)[["a"]] |> round(4)
      B  = coef(model)[["B"]] |> round(1)
      b  = coef(model)[["b"]] |> round(4)
      mgfr_2c = mgfr_2c |> round(1)
      mgfr_2c_bsa = mgfr_2c_bsa |> round(1)
      AUC_inf = AUC_inf |> round(1)
      model_r2 = model_r2 |> round(4)
      relmipirazin_vd = relmipirazin_vd |> round(2)
      legend("topright", adj=0.02, cex = 0.9,
             legend=c(bquote(C[Relm.]==.(A)*e^(-.(a)*t)+.(B)*e^(-.(b)*t)),
                      bquote(GFR==.(mgfr_2c)~mL/min),
                      bquote(GFR[bsa-adj]==.(mgfr_2c_bsa)~mL/min/1.73~m^2),
                      bquote(AUC[inf]==.(AUC_inf)~ug~min/L),
                      bquote(Vd==.(relmipirazin_vd)~L),
                      bquote(pseudo-R^2==.(model_r2))) )
    }
    plot_nls_fit(fit, time_min, relmipirazin)}
}
calculate_mgfr_relm(df_rel_a$time, df_rel_a$relmapirazin_ng_ml, height = 1.6002, weight = 71.698, output="plot")
calculate_mgfr_relm(df_rel_a$time, df_rel_a$relmapirazin_ng_ml, height = 1.6002, weight = 71.698, output="plot", nls_weights = FALSE)

df_relmapirazin |> 
  mutate(res = map(data, ~calculate_mgfr_relm(.x$time, .x$relmapirazin_ng_ml, height = 1.6002, weight = 71.698, output="summary"))) |> 
  unnest(res) |> View()

tabletools::df_relmapirazin
