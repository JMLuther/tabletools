#' Adjust p-value for an Hmisc::rcorr object
#' 
#' Adjust Hmisc::rcorr p-values using `p.adjust`
#'
#' @description Modifies the `rcorr` p-values from the `Hmisc::rcorr()` function.
#' The result is an array with modified `$P` matrix.
#' uses `base::p.adjust`
#' @param x An `rcorr` class object created by the function `Hmisc::rcorr`` 
#' @param method 
#' Uses "BH" by default
#' @return An rcorr array with the following matrices:
#' \itemize{
#'   \item correlation (`$r`). unchanged from the original
#'   \item n (`$n`). unchanged from the original
#'   \item P-value (`$P`). modified using the selected adjustment method
#' }
#' @seealso \code{\link[Hmisc]{rcorr}}, \code{\link[heatmaply]{heatmaply}}
#' @export
#' @examples
#' require(Hmisc)
#' require(Hmisc)
#' set.seed(2020) # arguably the worse seed ever
#' test_rcorr <- Hmisc::rcorr(matrix(runif(100), nrow = 10))
#' test_rcorr
#' 
#' # use p.adjust methods
#' rcorr_padjust(test_rcorr) # BH by default
#' rcorr_padjust(test_rcorr, method = "bonferroni") # BH by default
#' rcorr_padjust(test_rcorr, method = "holm") # BH by default
#' # SUMMARY TABLE OF RESULTS
#' # one unadjusted result has a low P-value (p=0.008)
#' method = c("none", "BH", "fdr", "holm", "hochberg", "hommel", "bonferroni", "BY")
#' data.frame(p.adjust.method = method,
#'            p.adjust = map_dbl(method, ~rcorr_padjust(test_rcorr, method = .x)$P[4,5]),
#'           txtPval = htmlTable::txtPval(map_dbl(method, ~rcorr_padjust(test_rcorr, method = .x)$P[4,5])))

rcorr_padjust <- function(x, method = "BH", ...) {
  stopifnot(class(x) == "rcorr")
  x$P[upper.tri(x$P)] <- p.adjust(x$P[upper.tri(x$P)], method = method)
  x$P[lower.tri(x$P)] <- p.adjust(x$P[lower.tri(x$P)], method = method)
  return(x)}

