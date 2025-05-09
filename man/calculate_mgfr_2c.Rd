% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_mgfr_2c.R
\name{calculate_mgfr_2c}
\alias{calculate_mgfr_2c}
\title{Iohexol measured GFR with 2-compartment model (early and late sampling)}
\usage{
calculate_mgfr_2c(
  time,
  iohexol_conc,
  omnipaque_v = 300,
  ioh_inj_vol = 5,
  ioh_inj_wt = NULL,
  ioh_units = "ug/mL",
  time_units = "min",
  id = NULL,
  height = NA,
  height_units = "m",
  weight = NA,
  weight_units = "kg",
  nls_weights = TRUE,
  nls_v = "gslnls",
  t_late = 120,
  t_early = 100,
  legend_cex = 1,
  output = "summary"
)
}
\arguments{
\item{time}{A vector of time values (minutes)}

\item{iohexol_conc}{A vector of Iohexol plasma measurements (ug/mL)}

\item{omnipaque_v}{Omnipaque version (eg 300 = Omnipaque 300 recommended).
Other versions commonly available include 300 and 350 (USA) and 140, 280,
240 (other areas)}

\item{ioh_inj_vol}{Iohexol injection Volume by syringe volume injected (Not
preferred; use if weights not available)}

\item{ioh_inj_wt}{Iohexol injection Weight by syringe weight determination
(Pre-Post weight difference; recommnded method) in grams}

\item{ioh_units}{Iohexol concentration units, defaults to \code{ug/mL}}

\item{time_units}{Time units, defaults to \code{min}}

\item{id}{Study identifier (optional, passed to plot title)}

\item{height}{Patient Height, m}

\item{height_units}{Height units, if not in m}

\item{weight}{Patient Weight, kg}

\item{weight_units}{Weight units, if not in kg}

\item{nls_weights}{Use weights in nls model (\code{TRUE} or \code{FALSE}), defaults to
\code{TRUE}. Uses 1/iohexol^2 as weights, which more heavily weight lower
concentrations obtained at later time points}

\item{nls_v}{Model estimation method (defaults to \code{gslnls}).  Can also use
\code{SI}, \code{base}, or \code{MSP}. If \code{MSP} is selected, only "late" time points will
be used and "early" time points will be discarded. If no early timepoints
are available, method defaults to \code{MSP}.}

\item{t_late}{First Time point (minutes) to use in the "Late" elimination
phase, defaults to \code{120}}

\item{t_early}{Last Time point (minutes) to use in the "Early" distribution
phase, defaults to \code{100}}

\item{legend_cex}{Magnification factor for plot text, default 1.0 (100\%)}

\item{output}{Desired output, defaults to \code{summary} of model. Alternatively
can specify \code{gfr}, \code{gfr_bsa}, \code{fit}, or \code{plot}

The \code{mgfr_method} describes the method for estimation, which can have the
following options:
\itemize{
\item \code{SI} Slope-Intercept method obtained by performing two linear regressions of log(iohexol) vs time for early and late time points. Obtained by setting \code{nls_v = "SI"}
\item \code{modified-SI} SI method with shared time-point(s) between early and late periods (ie overlapping \code{t_early} = \code{t_late}). Obtained by setting, for example \verb{nls_v = "SI", t_early=120, t_late=120} or another option with overlapping times.
\item \code{NLLS-gslnls-weighted} (Default method) NLS fit using the \code{gslnls()} function from the \code{gsl_nls} package. Obtained by setting \verb{nls_v = "gslnls", nls_weights=T}
\item \code{NLLS-base-weighted} NLS fit using the \code{nls()} function from the base \code{stats} package. Obtained by setting \verb{nls_v = "base", nls_weights=T}. Similar to \code{gslnls} method, except that parameter constraints cannot be set with \code{nls()}, and so implausible estimates (negative for a, b) may be obtained.
\item \code{NLLS-gslnls-unweighted} NLS fit using the \code{gslnls()} function from the \code{gsl_nls} package. Obtained by setting \verb{nls_v = "gslnls", nls_weights=F}
\item \code{NLLS-base-unweighted} NLS fit using the \code{nls()} function from the base \code{stats} package. Obtained by setting \verb{nls_v = "base", nls_weights=F}. This method weights heavily to early time points (higher concentration). Also, parameter constraints cannot be set with \code{nls()}, and so implausible estimates (negative for a, b) may be obtained.
\item \code{MSP-BM} The Multiple Sample Protocol (MSP), or Multiple Late Sample (MLS) estimate using 1-compartment analysis of Late time points only (>120 minutes post injection). Adjustment made using the Brochner-Mortenson correction equation to correct for unmeasured early time points.
\item \code{SS-Jacobbson} Single Sample estimate of GFR
}

For \code{output = "summary"} results returned includes the named variables:
\itemize{
\item \code{mgfr_method} Method used for GFR calculation (see above). Results should be similar.
\item \code{mgfr_2c} Measured GFR (mL/min)
\item \code{mgfr_2c_bsa} The measured GFR (mL/min/1.73 m2) indexed to body surface area (BSA calculated by DuBois equation)
\item \code{iohexol_m}   Iohexol mass (ucg) injected. Determined either by user-provided syringe weight or volume injected.
\item \code{iohexol_0}   Iohexol concentration at t=0 calculated by A+B (theoretical, not measured)
\item \code{iohexol_vd}  Iohexol Volume of distribution estimated by Dose/Iohexol\link{t0}
\item \code{ioh_auc}  Iohexol calculated AUC from t=0 to Infinity, estimated by A/a + B/b.
\item \code{A} Model parameter A
\item \code{a} Model parameter a, or \eqn{\alpha}
\item \code{B} Model parameter B
\item \code{b} Model parameter b, or \eqn{\beta}
\item \code{model_r2} Model Pseudo-R2, calculated as the linear regression R2 for predicted~observed values
\item \code{ssr} Sum of squared residuals
\item \code{sse} Sum of squared residuals for early time points
\item \code{ssl} Sum of squared residuals for late time points
\item \code{k10} Iohexol elimination rate constant from central compartment (1/min)
\item \code{k21} Iohexol transfer rate constant from peripheral to central compartment (1/min)
\item \code{k12} Iohexol transfer rate constant from central to peripheral compartment (1/min)
\item \code{n_early} Number of early time points measured and used in model
\item \code{n_late} Number of late time points measured and used in model
}

\code{gfr} returns a single \code{mgfr_2c} value, the measured GFR (mL/min).
\code{gfr_bsa} returns a single \code{mgfr_2c_bsa} value, the measured GFR
(mL/min/1.73 m2) indexed to body surface area (BSA calculated by DuBois
equation).

\code{fit} returns the non-linear regression model fit object.

\code{plot} returns a base-R plot of observed and predicted regression curve vs
time, and summary measures in the legend.}
}
\value{
Desired output with either a data.frame of results (\code{summary}), a
single value of BSA adjusted GFR (\code{gfr} or \code{gfr_bsa}), an \verb{nls object} for
model fit (\code{fit}), or a plot with observed values, model fit curve, and
summary results in the figure legend (\code{plot})
}
\description{
This method requires multiple early (alpha phase, typically <120 min) and
late (beta phase) sample measurements of iohexol to determine GFR (glomerular
filtration rate). This function uses nonlinear modeling via
Levenberg-Marquardt method in the \code{gsl_nsl} package and the \code{gslnls()}
function to fit the 2-compartment model of iohexol kinetics. The NLLS methods use the formula \eqn{C=A \cdot exp^{- \alpha \cdot t} +
B*e^{- \beta \cdot t} } and weighted by \eqn{1/Iohexol^2}. Initial estimates for early (A, a) and late (B, b) parameters used for NLLS estimation are obtained using standard "Slope-Intercept" methods for linear regresion of \eqn{log(Iohexol)~time} for two lines for the early and late portions. Results for this model can be obtained using the \code{nls_v="SI"} option. Use of the \code{gsl_nsl} package allows for better support of parameter constraints (constrained to positive values) and other methods (e.g. \code{predict} methods using fit object) compared to the base \code{nls()} function.  The general method and provided examples are
modified from \href{https://pubmed.ncbi.nlm.nih.gov/16612328/}{Schwartz et al.
Kidney Int. 2006.}. Further discussion and details regarding methods of fitting the 2-compartment models can be found at \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8101203/}{Pottel et al.} Results are returned as a non-indexed value for GFR (\code{mgfr_2c}) and also indexed to BSA of 1.73m2 if height and weight are provided (\code{mgfr_2c_bsa}).
The summary provides estimates of kinetic parameters (k10, k21, k12) which
can be used in ODE models (see example in \link{compare_mgfr_summary}), but is not
implemented here.
}
\examples{
library(tabletools)
# PUBLISHED IOHEXOL DATA ----
# data available in package by calling
## ├ Schwartz Data ----
# Data extracted from Schwartz Fig1 https://pubmed.ncbi.nlm.nih.gov/16612328/
# Iohexol 5mL IV injection (Omnipaque 300, 5mL ~3235mg Iohexol)
# sampling at 10, 20, 30, 60, 120, 240, 300, 360 min
# time (minutes)
# Iohexol (ug/ml)
# age, height, weight not known for the example
dat_schwartz

## ├ Pottel data ----
# from Supplemental document in https://pubmed.ncbi.nlm.nih.gov/33952185/
dat10
dat17

## ├ Tondel data ----
# full example data provided by Tondel in Table 2: https://pubmed.ncbi.nlm.nih.gov/29134449/
dat_tondel

# Comparison of Output options
calculate_mgfr_2c(dat_schwartz$time, dat_schwartz$iohexol_ug_ml, height = 1.67, weight = 70, ioh_inj_vol = 5) # Default
calculate_mgfr_2c(dat_schwartz$time, dat_schwartz$iohexol_ug_ml, height = 1.67, weight = 70, ioh_inj_vol = 5, output="gfr")
calculate_mgfr_2c(dat_schwartz$time, dat_schwartz$iohexol_ug_ml, height = 1.67, weight = 70, ioh_inj_vol = 5, output="gfr_bsa")
calculate_mgfr_2c(dat_schwartz$time, dat_schwartz$iohexol_ug_ml, height = 1.67, weight = 70, ioh_inj_vol = 5, output="fit")
calculate_mgfr_2c(dat_schwartz$time, dat_schwartz$iohexol_ug_ml, height = 1.67, weight = 70, ioh_inj_vol = 5, output="fit", nls_v="base")
calculate_mgfr_2c(dat_schwartz$time, dat_schwartz$iohexol_ug_ml, height = 1.67, weight = 70, ioh_inj_vol = 5, output="fit", nls_v="SI") # two fits

# Plot options: ideally pass the ID information
calculate_mgfr_2c(dat_schwartz$time, dat_schwartz$iohexol_ug_ml, height = 1.67, weight = 70, ioh_inj_vol = 5, output="plot", id="Name-IDnumber-Date")
calculate_mgfr_2c(dat_schwartz$time, dat_schwartz$iohexol_ug_ml, height = 1.67, weight = 70, ioh_inj_vol = 5, output="plot")
calculate_mgfr_2c(dat_schwartz$time, dat_schwartz$iohexol_ug_ml, height = 1.67, weight = 70, ioh_inj_vol = 5, output="plot", nls_v = "SI") # fit not found
calculate_mgfr_2c(dat_schwartz$time, dat_schwartz$iohexol_ug_ml, height = 1.67, weight = 70, ioh_inj_vol = 5, output="plot", nls_v = "SI", t_early = 120, t_late = 120)
calculate_mgfr_2c(dat_schwartz$time, dat_schwartz$iohexol_ug_ml, height = 1.67, weight = 70, ioh_inj_vol = 5, output="plot", nls_v = "gslnls")
calculate_mgfr_2c(dat_schwartz$time, dat_schwartz$iohexol_ug_ml, height = 1.67, weight = 70, ioh_inj_vol = 5, output="plot", nls_v = "base")
calculate_mgfr_2c(dat_schwartz$time, dat_schwartz$iohexol_ug_ml, height = 1.67, weight = 70, ioh_inj_vol = 5, output="plot", nls_v = "gslnls", nls_weights = F)
calculate_mgfr_2c(dat_schwartz$time, dat_schwartz$iohexol_ug_ml, height = 1.67, weight = 70, ioh_inj_vol = 5, output="plot", nls_v = "base", nls_weights = F)
calculate_mgfr_2c(dat_schwartz$time, dat_schwartz$iohexol_ug_ml, height = 1.67, weight = 70, ioh_inj_vol = 5, output="plot", legend_cex = 1.5)

# examples with fewer time points
dat_5p <- dat_schwartz[dat_schwartz$time \%in\% c(10, 20, 30, 120, 300), ]
calculate_mgfr_2c(dat_5p$time, dat_5p$iohexol_ug_ml, height = 1.67, weight = 70, ioh_inj_vol = 5, output="plot")

dat_5p <- dat_schwartz[dat_schwartz$time \%in\% c(10, 30, 60, 120, 300), ]
calculate_mgfr_2c(dat_5p$time, dat_5p$iohexol_ug_ml, height = 1.67, weight = 70, ioh_inj_vol = 5, output="plot")

dat_7p <- dat_schwartz[dat_schwartz$time \%in\% c(10, 20, 30, 60, 240, 300, 360), ]
calculate_mgfr_2c(dat_7p$time, dat_7p$iohexol_ug_ml, height = 1.67, weight = 70, ioh_inj_vol = 5, output="plot")

# if early time points not present, defaults to MSP-BM estimation (using `calculate_mgfr_msp` with warning)
dat_ebert
calculate_mgfr_2c(dat_ebert$time, dat_ebert$iohexol, height = 1.68, weight=87, ioh_inj_vol = 5.06)
calculate_mgfr_2c(dat_ebert$time, dat_ebert$iohexol, height = 1.68, weight=87, ioh_inj_vol = 5.06, output="plot", leg)
calculate_mgfr_msp(dat_ebert$time, dat_ebert$iohexol, height = 1.68, weight=87, ioh_inj_vol = 5.06)
}
\seealso{
\code{\link[=compare_mgfr_summary]{compare_mgfr_summary()}} for quick Table of results calculated by
available methods. \code{\link[=compare_mgfr_plot]{compare_mgfr_plot()}} for quick Visual comparison of
plots calculated by available methods.
}
