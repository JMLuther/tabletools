#' Summary text functions for html tables
#'
#' @param var A vector, or variable name within a dataframe
#' @param referent_value A character string to define the reference value
#' @param na.rm a logical value indicating whether NA values should be stripped
#' @export
#' @examples
#' library(dplyr)
#' txt_pct_fr(mtcars$gear, 4)
#' mtcars %>%
#'   group_by(cyl) %>%
#'   summarise(pct_4gears = txt_pct_fr(gear, 4)) # shows what percentage have 4 gears (selected reference) and (with fraction)

txt_pct_fr <- function(var, referent_value, na.rm = TRUE){
  sprintf("%.1f%%(<sup>%.0f</sup>&frasl;<sub>%.0f</sub>)",
          mean(var == referent_value, na.rm = na.rm)*100,
          sum(var == referent_value, na.rm = na.rm), length(var))
  }

txt_pct_fr2 <- function(var, referent_value, na.rm = TRUE){
  sprintf("%.2f%%(<sup>%.0f</sup>&frasl;<sub>%.0f</sub>)",
          mean(var == referent_value, na.rm = na.rm)*100,
          sum(var == referent_value, na.rm = na.rm), length(var))
}

