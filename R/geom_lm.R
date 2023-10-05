#' linear regression version of geom_smooth()
#'
#' Convenience function- a version of `geom_smooth()` using `lm()` instead of `loess()`
#'
#' @param formula y~x for most
#' @param se Plot error range (T/F)
#' @param linewidth width of regression line
#' @param ... pass additional arguments
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' Orange %>% 
#'   ggplot(aes(age, circumference)) +
#'     geom_lm(alpha = 0.5, color = "steelblue") +
#'       geom_point() 

geom_lm <- function(formula = y ~ x, se = TRUE, 
                    linewidth = 1, ...)  {
  geom_smooth(formula = formula, se = se, method = "lm", 
              linewidth = linewidth, ...)}
