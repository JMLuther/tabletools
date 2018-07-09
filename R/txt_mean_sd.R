#' Summary text functions for html tables
#'
#' @param var A numeric vector or variable name within a dataframe
#' @param na.rm a logical value indicating whether NA values should be stripped
#' @param ... Values to be passed into `mean` arguments.
#' @usage txt_mean_sd(var, na.rm = TRUE, ...)
#' @return A character vector with mean and sd, suitable for printing in an html table
#' @seealso  \code{\link[base]{sprintf}}, \code{\link[base]{mean}}, \code{\link[base]{sd}}
#' @export
#' @examples
#' library(dplyr)
#' txt_mean_sd(mtcars$mpg)
#' txt_mean_sd(mtcars$mpg, trim = .2)
#' txt_mean_sem(mtcars$mpg)
#' txt_mean_range(mtcars$mpg)
#' txt_median_iqr(mtcars$mpg)
#' txt_pct_fr(mtcars$cyl, 6)
#' mtcars %>%
#'   group_by(cyl) %>%
#'   summarise(mean_mpg = txt_mean_sd(mpg))

txt_mean_sd <- function(var, na.rm = T, ...){
  sprintf("%.1f &plusmn; %.1f",
          mean(var, na.rm = na.rm, ...),
          sd(var, na.rm = na.rm)
          )
}
