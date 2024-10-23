#' Compare Iohexol mGFR results using available methods
#'
#' Useful to analyze outlier or inconsistent results. Note most variation occurs
#' due to poor fitting of early timepoints. Further discussion and details
#' regarding methods of fitting the 2-compartment models can be found at
#' \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8101203/}{Pottel et al.}
#' The summary provides estimates of kinetic parameters (k10, k21, k12) which
#' can be used in ODE models, but is not implemented here.
#'
#' @inheritParams calculate_mgfr_2c
#' @param time
#'
#' @return `compare_mgfr_summary()` returns a Table of mGFR summary values, using combination of available methods
#' @export
#' @examples
#' library(tabletools)
#' # PUBLISHED IOHEXOL DATA ----
#' data available in package by calling 
#' ## ├ Schwartz Data ----
#' # Data extracted from Schwartz Fig1 https://pubmed.ncbi.nlm.nih.gov/16612328/
#' # Iohexol 5mL IV injection (Omnipaque 300, 5mL ~3235mg Iohexol)
#' # sampling at 10, 20, 30, 60, 120, 240, 300, 360 min
#' # time (minutes)
#' # Iohexol (ug/ml)
#' # age, height, weight not known for the example
#' dat_schwartz
#' 
#' ## ├ Pottel data ----
#' # from Supplemental document in https://pubmed.ncbi.nlm.nih.gov/33952185/
#' dat10
#' dat17
#'
#' ## ├ Tondel data ----
#' # full example data provided by Tondel in Table 2: https://pubmed.ncbi.nlm.nih.gov/29134449/
#' dat_tondel
#' 
#' compare_mgfr_summary(dat_tondel$time, dat_tondel$iohexol, ioh_inj_vol = 2.08, weight = 13, height = 0.9)
#' compare_mgfr_plot(dat_tondel$time, dat_tondel$iohexol, ioh_inj_vol = 2.08, weight = 13, height = 0.9)
#' 
#' # Comparisons of available methods: published studies
#' # Schwartz data in kids; note don't have subject details (height, weight, inj_vol)
#' compare_mgfr_summary(dat_schwartz$time, dat_schwartz$iohexol_ug_ml, height = 0.8, weight = 13, ioh_inj_vol = 2.08)
#' compare_mgfr_plot(dat_schwartz$time, dat_schwartz$iohexol_ug_ml, height = 0.8, weight = 13, ioh_inj_vol = 2.08)
#' 
#' # Pottel ID#10 Difficult fit
#' # Negative value using unconstrained parameters (NLLS-base-unweighted)
#' # caused by early outlier data points (t=60 most likely)
#' compare_mgfr_summary(dat10$time, dat10$conc, height = 1.67, weight = 70)
#' compare_mgfr_plot(dat10$time, dat10$conc, height = 1.67, weight = 70)
#' 
#' # Pottel ID#17 Difficult fit
#' compare_mgfr_summary(dat17$time, dat17$conc, height = 1.67, weight = 70)
#' compare_mgfr_plot(dat17$time, dat17$conc, height = 1.67, weight = 70)
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
#' ioh_nlfit <- calculate_mgfr_2c(dat_schwartz$time, dat_schwartz$iohexol_ug_ml, height = 1.67, weight = 70, ioh_inj_vol = 5)
#' pars  <- c(k10 = ioh_nlfit$k10, k21 = ioh_nlfit$k21, k12 = ioh_nlfit$k12)
#' # plug into the ODE model solution
#' yini  <- c(Iohexol = ioh_nlfit$A+ioh_nlfit$B, C2=0)
#' times <- seq(0, 360, by = 1)
#' out   <- ode(yini, times, m2c, pars)
#' out_df <- as.data.frame(out)
#' # Plot to verfiy kinetic parameters vs nls model
#' calculate_mgfr_2c(dat_schwartz$time, dat_schwartz$iohexol_ug_ml, height = 1.67, weight = 70, ioh_inj_vol = 5, output="plot")
#' lines(out_df$time, out_df$Iohexol, lty=2)
  
compare_mgfr_summary <- function(time, iohexol_conc, height, weight, ioh_inj_vol=5, t_early=100, t_late=120){
  rbind(
    calculate_mgfr_2c(time, iohexol_conc, output="summary", height=height, weight=weight, ioh_inj_vol = ioh_inj_vol, nls_v = "SI"),
    calculate_mgfr_2c(time, iohexol_conc, output="summary", height=height, weight=weight, ioh_inj_vol = ioh_inj_vol, nls_v = "SI", t_early = t_early, t_late = t_late),
    calculate_mgfr_2c(time, iohexol_conc, output="summary", height=height, weight=weight, ioh_inj_vol = ioh_inj_vol, nls_v = "gslnls"),
    calculate_mgfr_2c(time, iohexol_conc, output="summary", height=height, weight=weight, ioh_inj_vol = ioh_inj_vol, nls_v = "base"),
    calculate_mgfr_2c(time, iohexol_conc, output="summary", height=height, weight=weight, ioh_inj_vol = ioh_inj_vol, nls_v = "gslnls", nls_weights = F),
    calculate_mgfr_2c(time, iohexol_conc, output="summary", height=height, weight=weight, ioh_inj_vol = ioh_inj_vol, nls_v = "base", nls_weights = F)
  )
}

#' @inheritParams calculate_mgfr_2c
#' @return `compare_mgfr_plot()` returns a 3x2 grid of mGFR summary Plots, using combination of available methods
#' @export
#' @rdname compare_mgfr_summary
compare_mgfr_plot <- function(time, iohexol_conc, height, weight, ioh_inj_vol=5, t_common=120,...){
  if (is.null(t_common)) {stop("Provide a value for 't_common' for m-SI method")}
  opar_mfr = par("mfrow")
  par(mfrow = c(3, 2))
  calculate_mgfr_2c(time, iohexol_conc, output="plot", height=height, weight=weight, ioh_inj_vol = ioh_inj_vol, nls_v = "SI")
  calculate_mgfr_2c(time, iohexol_conc, output="plot", height=height, weight=weight, ioh_inj_vol = ioh_inj_vol, nls_v = "SI", t_early = 120, t_late = 120)
  calculate_mgfr_2c(time, iohexol_conc, output="plot", height=height, weight=weight, ioh_inj_vol = ioh_inj_vol, nls_v = "gslnls")
  calculate_mgfr_2c(time, iohexol_conc, output="plot", height=height, weight=weight, ioh_inj_vol = ioh_inj_vol, nls_v = "base")
  calculate_mgfr_2c(time, iohexol_conc, output="plot", height=height, weight=weight, ioh_inj_vol = ioh_inj_vol, nls_v = "gslnls", nls_weights = F)
  calculate_mgfr_2c(time, iohexol_conc, output="plot", height=height, weight=weight, ioh_inj_vol = ioh_inj_vol, nls_v = "base", nls_weights = F)
  par(mfrow=opar_mfr)
}

