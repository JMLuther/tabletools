#' Calculate Mean Arterial Pressure
#'
#' Returns the mean arterial pressure (MAP) from systolic (SBP) and diastolic
#' (DBP) blood pressure measurements, using several methods. For brachial BP measurements, the formula \eqn{MAP=DBP + 0.4 \cdot
#' PP} as described in \href{https://pubmed.ncbi.nlm.nih.gov/17351365/}{Bos 2007} is typically preferred ("standard40"). Note the commonly used estimate of adding 33% of PP has been shown to underestimate MAP, although variable peripheral pulse amplification makes this difficult to estimate accurately.  Heart rate adjusted
#' methods reported by \href{https://pubmed.ncbi.nlm.nih.gov/1860182/}{Gomez}
#' and by \href{https://pubmed.ncbi.nlm.nih.gov/15558774/}{Razminia} are also
#' supported. The Gomez method is applied as the default, for use in calculation
#' in renal glomerular pressure estimates.
#'
#' @param sbp Systolic blood pressure, mmHg
#' @param dbp Diastolic blood pressure, mmHg
#' @param hr Heart rate, bpm
#' @param method MAP formula, one of `gomez`, `standard33`, `standard40`, or `razminia`. Uses
#'   "gomez" method by default. If no HR is supplied, the "standard" method is
#'   used. If `standard` method is used without specifying it, due to no HR, a
#'   warning is issued.
#'
#' @return map as a single value, or vector of results
#' @export calculate_map
#'
#' @examples
#' calculate_map(159, 46)
#' calculate_map("159", 46)
#' calculate_map("159", NA)
#' calculate_map(140, 90)
#' calculate_map(140, 90, 70, method="standard33")
#' calculate_map(140, 90, 70, method="standard40")
#' calculate_map(140, 90, 70, method = "gomez")
#' calculate_map(140, 90, 70, method="razminia")
#'
#' hr <- 40:125
#' library(purrr)
#' plot(hr, map_dbl(hr, ~calculate_map(125, 73, .x, method = "gomez")), "l", lty=1, col="black",
#'      xlim = c(35,127.5), ylim = c(90,99),
#'      ylab = "Calculated MAP (mmHg)")
#' title("Methods comparison for HR-adjusted MAP calculation")
#' lines(hr, map_dbl(hr, ~calculate_map(125, 73, .x, method = "razminia")), lty=2, col="red")
#' abline(h=calculate_map(125,73, method="standard33"),lty=2, col="blue")
#' abline(h=calculate_map(125,73, method="standard40"),lty=2, col="steelblue")


calculate_map <- function(sbp, dbp, hr=NA, method="gomez"){ 
  if (any(is.na(sbp) | any(is.na(dbp)))) {
    rlang::warn("Check for missing values in sbp, dbp")
    return(NA_real_)}
  
  if (any(!is.numeric(sbp)) | any(!is.numeric(dbp))) {
    rlang::warn("non-numeric values in sbp or dbp")
    return(NA_real_)}
  
  if (method == "standard40"){
    map = dbp + 0.4*(sbp-dbp)
    return(map)
  }
  if (method == "standard33"){
    map = 1/3*sbp + 2/3*dbp
    return(map)
  }
  
  if (is.na(hr)){
    rlang::warn("no HR value, defaulting to method='standard40'")
    map = dbp + 0.4(sbp-dbp)
    return(map)
  }
  if (method=="gomez"){
    # for method see https://pubmed.ncbi.nlm.nih.gov/1860182/
    # rho = fraction of PP ~ mean value during diastole
    gamma = 2.3*sqrt(hr)/60
    rho = 12*(3*gamma + 1)/100
    k = 0.77*gamma + rho*(1-gamma)
    map = dbp + k*(sbp-dbp)
    return(map)
  }
  if (method == "razminia"){ 
    # see https://pubmed.ncbi.nlm.nih.gov/15558774/
    map = dbp + (0.33 + (hr*0.0012))*(sbp-dbp)
    return(map)
  } else {return(NA)}
}
