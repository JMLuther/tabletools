#' Summary text functions for html tables
#'
#' @inheritParams txt_mean_sd
#' @return A character vector with mean (range), suitable for printing in an html table
#' @seealso  \code{\link[base]{sprintf}}, \code{\link[base]{mean}}, \code{\link[base]{min}}, \code{\link[base]{max}}
#' @export
#' @examples
#' library(dplyr)
#' txt_mean_range(mtcars$mpg)
#' mtcars %>%
#'   group_by(cyl) %>%
#'   summarise(mean_mpg = txt_mean_range(mpg))

txt_mean_range <- function(var, na.rm = TRUE,  digits=1, unicode=T, ...){
  string = paste0("%1.", digits, "f(%.", digits, "f-%.", digits, "f)")
  sprintf("%.1f(%.1f-%.1f)",
          mean(var, na.rm = na.rm),
          min(var, na.rm = na.rm),
          max(var, na.rm = na.rm))
}
