#' Summary bar plot (with optional error bar, overlying points)
#'
#' Plots a summary Barplot of the averaged group values. Optionally can add
#' error bars (SEM) and jittered data points. Does not have full functionality
#' of `geom_bar()`, e.g., cannot pass additional aes() elements or arguments.
#'
#' @param se add SEM bars (TRUE/FALSE)
#' @param points add jittered points (uses `ggbeeswarm::geom_quasirandom()`)
#'   (TRUE/FALSE)
#' @param barfill color for bar fill (as a string, default = "steelblue")
#'
#' @return a ggplot object
#' @export
#'
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(factor(cyl), disp))
#' p +  geom_barmean()
#' p +  geom_barmean(points = T)
#' p +  geom_barmean(points = T, barfill = "purple") 

geom_barmean <- function(se = TRUE, points = F, barfill = "steelblue"){
  list(
    stat_summary(fun = "mean", geom = "bar", fill = barfill, alpha=.5),
    if (se) 
      stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width=0.25, linewidth = 1),
    if (points)
      ggbeeswarm::geom_quasirandom()
    )
  }

