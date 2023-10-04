#' ggplot group summary stat with line, point and error bar
#'
#' @param line add connecting lines (TRUE/FALSE)
#' @param pd amount to use for dodge (use if multiple groups/lines)
#' @param fun summary function used (default = "mean")
#' @param error add SEM bars (TRUE/FALSE)
#' @param point add points (TRUE/FALSE)
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' library(ggplot2)
#' # Tree Growth
#'
#' # just summary stats
#' p <- ggplot(Orange, aes(x = age, y = circumference)) + 
#'   geom_pointline_sem(pd=0) +
#'   theme_classic()
#' p 
#' # add individual data
#' p + geom_line(aes(x = age, y = circumference, group = Tree), inherit.aes = F, color = "black", alpha =0.3) 
#' 
#' # Indomethacin PK
#' p <- ggplot(Indometh, aes(x = time, y = conc)) + 
#'   geom_pointline_sem(pd=0) +
#'   scale_y_log10()
#'   p
#'  p + geom_line(aes(x = time, y = conc, group = Subject), inherit.aes = F, color = "black", alpha =0.3)

geom_pointline_sem <- function(point = T, line=T, pd = 0, fun = "mean", error = T){
  pos.dodge = position_dodge(pd) # adjust for non-overlapping lines
  # could alternatively use fun = "median"
  list(
    if (point)
    stat_summary(fun = fun, geom = "point", position = pos.dodge, shape = 21),
    if (line)
      stat_summary(fun = fun, geom = "line", position = pos.dodge),
    if (error) # mean +/- SEM
      stat_summary(fun.data = "mean_se", geom = "errorbar", width=0.3, linewidth = 1, position = pos.dodge)
  )
}
