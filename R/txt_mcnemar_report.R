#' Summary text functions for html tables
#'
#' @description This function returns html formatted text summary of McNemar's Chi-sqauare test, for paired data. 
#' The input object must be a `mcnemar.test` object, and the p-value is formatted by the 
#' `htmlTable::txtPval()` function.  
#' 
#' @param mcnemar_object A McNemar's Chi-square object 
#'
#' @export
#' @seealso \code{\link[htmlTable]{txtPval}}, \code{\link[base]{sprintf}}, \code{\link[stats]{mcnemar.test}}
#' @examples
#' ex <- data.frame(A = sample(c("No", "Yes"), 9, replace = T),
#'                  B = sample(c("No", "Yes"), 9, replace = T))
#' ex
#' result <- mcnemar.test(table(ex$A, ex$B))
#' txt_mcnemar_report(result)


txt_mcnemar_report <- function (mcnemar_object) {
  sprintf("McNemar's <i>&Chi;<sup>2</sup></i>(df=%1.0f)=%1.0f, <i>p</i>=%s", 
          mcnemar_object$statistic,
          mcnemar_object$parameter,
          htmlTable::txtPval(mcnemar_object$p.value))}


