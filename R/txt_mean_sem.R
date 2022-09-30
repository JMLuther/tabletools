#' Summary text functions for html tables
#'
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
txt_mean_sem <- function(var, na.rm = TRUE, ...){
  sprintf("%.1f&plusmn;%.1f",
          mean(var, na.rm = na.rm),
          sd(var, na.rm = na.rm)/sqrt(length(var)))
}

txt_mean_sem2 <- function(var, na.rm = TRUE, ...){
  sprintf("%.2f&plusmn;%.2f",
          mean(var, na.rm = na.rm),
          sd(var, na.rm = na.rm)/sqrt(length(var)))
}
