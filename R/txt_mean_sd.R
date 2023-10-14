#' Summary text functions for html tables
#'
#' @param var A numeric vector or variable name within a dataframe
#' @param digits number of decimal places (limited to 1-3)
#' @param na.rm a logical value indicating whether NA values should be stripped
#'
#' @return A character vector with mean and sd, suitable for printing in an html table
#' @seealso  \code{\link[base]{sprintf}}, \code{\link[base]{mean}}, \code{\link[base]{sd}}
#' @export
#' @examples
#' library(dplyr)
#' txt_mean_sd(1:10)
#' txt_mean_sd(1:10, digits = 1)
#' txt_mean_sd(1:10, digits = 3)

txt_mean_sd <- function (var, na.rm = TRUE, digits=2) {
  switch (digits,
          `1` =   sprintf("%.1f&plusmn;%.1f", mean(var, na.rm = na.rm), 
                          sd(var, na.rm = na.rm)),
          `2` =  sprintf("%.2f&plusmn;%.2f", mean(var, na.rm = na.rm), 
                         sd(var, na.rm = na.rm)),
          `3` =  sprintf("%.3f&plusmn;%.3f", mean(var, na.rm = na.rm), 
                         sd(var, na.rm = na.rm))
  )
}
