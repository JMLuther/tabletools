#' Summary text functions for html tables
#'
#' @param var A numeric vector or variable name within a dataframe
#' @param digits number of decimal places (limited to 1-3)
#' @param na.rm a logical value indicating whether NA values should be stripped
#'
#' @return A character vector with mean and sd, suitable for printing in an html table
#' @seealsodata:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAWElEQVR42mNgGPTAxsZmJsVqQApgmGw1yApwKcQiT7phRBuCzzCSDSHGMKINIeDNmWQlA2IigKJwIssQkHdINgxfmBBtGDEBS3KCxBc7pMQgMYE5c/AXPwAwSX4lV3pTWwAAAABJRU5ErkJggg==  \code{\link[base]{sprintf}}, \code{\link[base]{mean}}, \code{\link[base]{sd}}
#' @export
#' @examples
#' library(dplyr)
#' txt_mean_sd(1:10)
#' txt_mean_sd(1:10, digits = 1)
#' txt_mean_sd(1:10, digits = 3)
#' txt_mean_sd(x, digits = 3, unicode = F)


txt_mean_sd <- function (var, na.rm = TRUE, digits=2, unicode=T) {
  if (unicode) { 
    string = paste0("%1.", digits, "f\U00B1%1.", digits, "f")
  } else {string = paste0("%1.", digits, "f&plusmn;%1.", digits, "f")}
  sprintf(string, mean(var, na.rm = na.rm), 
          sd(var, na.rm = na.rm))
}
