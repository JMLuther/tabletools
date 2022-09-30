#' Get p-value from an rms::Gls model
#'
#' @param gls_model a Gls model obtained from \code{\link[rms]{Gls}}
#' @return A single p-value
#' @seealso  \code{\link[rms]{Gls}}
#' @export


gls_pvalue <- function(gls_model) {
  p.value = 2 * pt(-abs(coef(gls_model)/sqrt(diag(as.matrix(gls_model$varBeta)))), gls_model$dims$N - gls_model$dims$p)[2] 
  names(p.value) = "p.value"
  return(p.value)
}
