#'Oral Glucose Tolerance Datasets
#'
#'@description Data obects containing results from stanadard 75g OGTT including
#'  glucose, insulin and demographic data.
#'
#'  All OGTT studies were done after an overnight fast, and all glucose results
#'  are plasma glucose (mg/dL) measured on a YSI instrumement (glucose oxidase).
#'  Plasma Insulin (mU/L or uU/mL). All OGTTs were dones prior to any
#'  intervention (ie, participants were not on study medications).
#'
#'@format `ogtt_wide` A dataframe with 184 OGTT results contains glucose and
#'  insulin in wide format with time noted in the variable name (e.g.,
#'  `glucose_000`, `glucose_030`). `ogtt_nested` contains time, glucose, and
#'  insulin in long format a dataframe stored in a list-column for each
#'  participant.
#'  Demographic data includes:
#' \describe{
#' \item{age}{Age, years}
#' \item{gender.f}{Gender, factored as Male/Female}
#' \item{race.f}{Race, factored using self-reported classification]}
#' \item{height_cm}{Height, cm}
#' \item{weight_kg}{Weight, kg}
#' \item{bmi}{Body mass index (BMI, \eqn{kg/m^2})}
#' \item{`hdl_bl`, `dld_bl`, `trigs_bl`}{Fasting lipids: LDL and HDL cholesterol, Triglycerides (mg/dL)}
#' \item{`glucose_***`}{Plasma Glucose (mg/dL), with time in minutes}
#' \item{`insulin_***`}{Plasma Insulin (uU/mL), with time in minutes}
#' \item{`ogtt_df`}{A nested dataframe with a single OGTT result per row in the nested dataset (`ogtt_nested`), Time points, in minutes (0, 30, 60, 90, 120) denoted either in the variable name (`_000`, `_030`) or in the time column of the nested dataset}
#'}
#'@source Primary data collected from multiple studies
#'@md 

"ogtt_wide"

#' @rdname ogtt_wide
#' @format `ogtt_wide` A dataframe with 184 OGTT results, with a nested
#'   dataframe with each participant result (time, glucose, insulin)
"ogtt_nested"
