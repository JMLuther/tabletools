#' Summary text functions for html tables
#'
#' @inheritParams txt_mean_sd
#' @return A character vector with mean and sem, suitable for printing in an html table
#' @seealso  \code{\link[base]{sprintf}}, \code{\link[base]{mean}}, \code{\link[base]{sd}}
#' @export
#' @examples
#' txt_mean_sem(mtcars$mpg)
#' txt_mean_sem(mtcars$mpg, digits=1)

txt_mean_sem <- function (var, na.rm = TRUE, digits=2, unicode=T) {
  if (unicode) { 
    string = paste0("%1.", digits, "f\U00B1%1.", digits, "f")
  } else {string = paste0("%1.", digits, "f&plusmn;%1.", digits, "f")}
  sprintf(string, mean(var, na.rm = na.rm), 
          sd(var, na.rm = na.rm)/sqrt(length(var)))
}
