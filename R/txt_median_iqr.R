#' Summary text functions for html tables
#'
#' @param var A vector, or variable name within a dataframe
#' @param lower For quantile functions, the lower quantile (0.25 default)
#' @param upper For quantile functions, the upper quantile (0.75 default)
#' @param na.rm a logical value indicating whether NA values should be stripped
#' @return A character vector with median (interquartile range), suitable for printing in an html table
#' @seealso  \code{\link[base]{sprintf}}, \code{\link[stats]{median}}, \code{\link[base]{quantile}}
#' @export
#' @examples
#' library(dplyr)
#' txt_median_iqr(mtcars$mpg)
#' mtcars %>%
#'   group_by(cyl) %>%
#'   summarise(mean_mpg = txt_mean_range(mpg))

txt_median_iqr <- function(var, lower=0.25, upper=0.75, na.rm = T, ...){
  sprintf("%.1f (%.1f - %.1f)",
          median(var, na.rm = na.rm, ...),
          quantile(var, lower, na.rm = na.rm, names = FALSE),
          quantile(var, upper, na.rm = na.rm, names = FALSE)
  )
}
