#' Custom Summarization for a variable in a dataframe
#'
#' @description Uses `dplyr` style summarization to retrun mean, SD, n, SEM, and 95% CI for a
#' single continuous variable in a dataframe. Useful for grouped data summaries.
#' @param df A dataframe
#' @param my_var A variable name
#' @param probs For quantile functions, a vector with the desired quantiles...c(0.025, 0.975) default
#' @param na.rm a logical value indicating whether NA values should be stripped
#' @param digits integer indicating number of decimal places
#' @return A dataframe with named variables for: mean, sd, n, sem, and 95%CI
#' @seealso \code{\link[dplyr]{summarise}}, \code{\link[base]{mean}}, \code{\link[base]{sd}},
#'  \code{\link[base]{round}}, \code{\link[stats]{quantile}}
#' @export
#' @examples
#' library(dplyr)
#' my_summary(mtcars, mpg)
#' mtcars %>% group_by(cyl) %>% my_summary(mpg)
#' mtcars %>% group_by(cyl) %>% my_summary(mpg, probs = c(0.01, .999), digits = 0)

my_summary <- function(df, my_var, probs = c(0.025, .975), na.rm = T, digits = 2, ...) {
  my_var <- enquo(my_var)
  mean_name <- paste0(quo_name(my_var), "_mean")
  median_name <- paste0(quo_name(my_var), "_median")
  sd_name   <- paste0(quo_name(my_var), "_sd")
  n_name    <- paste0(quo_name(my_var), "_n")
  sem_name  <- paste0(quo_name(my_var), "_sem")
  cil_name  <- paste0(quo_name(my_var), "_ci_", gsub("^[0-9]+.([0-9]+)", "\\1", probs[[1]])) # allows for flexible quantiles
  ciu_name  <- paste0(quo_name(my_var), "_ci_", gsub("^[0-9]+.([0-9]+)", "\\1", probs[[2]])) # allows for flexible quantiles

  summarise(df,
            !!n_name    := sum(!is.na(!!my_var)), # an integer
            !!mean_name := round(mean(!!my_var, na.rm = na.rm), digits),
            !!sd_name   := round(sd(!!my_var, na.rm=na.rm), digits),
            !!sem_name  := round(sd(!!my_var, na.rm=na.rm)/sqrt(sum(!is.na(!!my_var))), digits),
            !!median_name := round(median(!!my_var, na.rm = na.rm), digits),
            !!cil_name  := round(quantile(!!my_var, probs = probs[[1]], names = F, na.rm = na.rm), digits),
            !!ciu_name  := round(quantile(!!my_var, probs = probs[[2]], names = F, na.rm = na.rm), digits)
  )
}
