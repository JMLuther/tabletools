#' ggplot group summary stat with line, point and error bar
#'
#' @param line add connecting lines (TRUE/FALSE)
#' @param pd amount to use for dodge (use if multiple groups/lines)
#' @param error add SEM bars (TRUE/FALSE)
#' @param point add points (TRUE/FALSE)
#' @param point_fill fill color for points (default = "red")
#' @param line_color line color (default = "red")
#' @param linewidth width for line and errorbar line 
#' @param errorbarwidth width of 
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

geom_pointline_sem <- function(point = T, point_fill = "red",
                               line=T, line_color = "red", linewidth = 1, 
                               error = T, errorbarwidth = 0.2, 
                               pd = 0){
  pos.dodge = position_dodge(pd) # adjust for non-overlapping lines
  # could alternatively use fun = "median"
  list(
    if (point)
      stat_summary(fun = "mean", geom = "point", position = pos.dodge, shape = 21, fill = point_fill),
    if (line)
      stat_summary(fun = "mean", geom = "line", position = pos.dodge, color = line_color,
                   linewidth = linewidth),
    if (error) # mean +/- SEM
      stat_summary(fun.data = "mean_se", geom = "errorbar", width=errorbarwidth, linewidth = 1, position = pos.dodge, color = line_color
      )
  )
}
