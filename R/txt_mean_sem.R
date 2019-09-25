#' Summary text functions for html tables
#'
#' @usage txt_mean_sem(var, na.rm = TRUE, ...)
#' @inheritParams txt_mean_sd
#' @return A character vector with mean and sem, suitable for printing in an html table
#' @seealso  \code{\link[base]{sprintf}}, \code{\link[base]{mean}}, \code{\link[base]{sd}}
#' @export
#' @examples
#' library(dplyr)
#' txt_mean_sd(mtcars$mpg)
#' txt_mean_sem(mtcars$mpg)
#' mtcars %>%
#'   group_by(cyl) %>%
#'   summarise(mean_mpg = txt_mean_sem(mpg))
txt_mean_sem <- function(var, ...){
  sprintf("%.1f&plusmn;%.1f",
          mean(var, na.rm = T),
          sd(var, na.rm = T)/sqrt(length(var)))
}

