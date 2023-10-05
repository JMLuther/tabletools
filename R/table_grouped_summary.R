#' Grouped Summary Table with optional html output
#'
#' @param df a dataframe
#' @param var_names a character vector containing bare variable names
#' @param grouping_variable a character string containing the grouping variable name
#' @param html_output should the result be returned as an htmlTable (if `html_table`` = TRUE) or as a dataframe (default)  
#' @param total should an overal total column be included (default) or omitted
#' @return A text table with grouped summary
#' @seealso  \code{\link[base]{sprintf}}, \code{\link[base]{mean}}, \code{\link[stats]{sd}}, \code{\link[htmlTable]{htmlTable}} 
#' @export
#' @examples
#' library(tidyverse)
# # base(ish) R example 
# # doesn't include the total
# aggregate(mtcars[, c("disp", "hp", "drat", "mpg", "wt")],
#'           by = list(cyl = mtcars[, "cyl"]),
#'           FUN = function(x) sprintf("%.2f &pm %.2f", mean(x), sd(x))) %>% 
#'   t() %>% 
#'   as.data.frame() %>% 
#'   rownames_to_column() %>% 
#'   {names(.) <- unlist(.[1, ]); .} %>% 
#'   .[-1, ] 
#' 
#' # output HTML format
#' mtcars %>% 
#'   table_grouped_summary(., 
#'                         var_names =  c("disp", "hp", "drat", "mpg", "wt"),
#'                         grouping_variable = "cyl",
#'                         html_output = TRUE,
#'                         total = TRUE)
#' 
#' # output text format
#'   mtcars %>% 
#'   table_grouped_summary(., 
#'                         var_names =  c("disp", "hp", "drat", "mpg", "wt"),
#'                         grouping_variable = "cyl",
#'                         html_output = FALSE,
#'                         total = TRUE)
#' 
#' # without overall total
#' mtcars %>% 
#'   table_grouped_summary(., 
#'                         var_names =  c("disp", "hp", "drat", "mpg", "wt"),
#'                         grouping_variable = "cyl",
#'                         html_output = FALSE,
#'                         total = FALSE)
 

table_grouped_summary <- function(df, var_names, grouping_variable, html_output = FALSE, total = TRUE) {
  t_sub =
    t(aggregate(df[, var_names], 
                by = list(group = df[, grouping_variable]), 
                FUN = function(x) sprintf("%0.2f &pm; %0.2f", 
                                          mean(x, na.rm = T), 
                                          sqrt(var(x, na.rm = T)/sum(!is.na(x))))))
  ifelse(total == TRUE, {
    t_total =
      as.matrix(unlist(lapply(df[, var_names],
                              FUN = function(x) sprintf("%0.2f &pm; %0.2f",
                                                        mean(x, na.rm = T),
                                                        sqrt(var(x, na.rm = T)/sum(!is.na(x)))))))
    t_total =
      rbind(c("TOTAL"),
            t_total)
    t_out = as.data.frame(cbind(t_sub, t_total))},
    {t_out = as.data.frame(t_sub)})
  colnames(t_out) <- unlist(t_out[1, ])
  t_out <-  t_out[-1, ]
  t_out <-  rownames_to_column(t_out,
                               var = "measure")
  ifelse(html_output == FALSE, 
         return(t_out), 
         return(htmlTable::htmlTable(t_out, rnames=FALSE,
                                     css.cell = rbind(rep("font-size: 1em; padding-left: .5em; padding-right: .5em;", times=ncol(t_out)),
                                                      matrix("padding:0 5px 0 5px;", ncol=ncol(t_out), nrow=nrow(t_out))),
                                     ...)))
}

