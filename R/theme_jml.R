#' my preferred basic ggplot theme
#' R code for my preferred ggplot theme
#' 
#' Can be called as a function from this package, or can be loaded from a Github gist containing the same code.
#'
#' @export
#' @examples
#' library(tidyverse)
#' pl_mtc <- mtcars |> 
#'  ggplot(aes(cyl, mpg)) +
#'  geom_point()
#'  
#'  pl_mtc + theme_gray()
#'  pl_mtc + theme_classic()
#'  pl_mtc + theme_jml()
#'
#' #OGTT example:
#' data("ogtt_nested")
#' ogtt_long <- 
#'  ogtt_nested |> 
#'  select(id, ogtt_df) |> 
#'  unnest(cols = ogtt_df) |> 
#'  group_by(id)
#'
#'pl_ogtt <- ogtt_long |> 
#'  ggplot(aes(time, glucose, group=id)) +
#'  geom_line(alpha=0.3)
#'  
#'  pl_ogtt + theme_jml()
#'  
#' #' to make this the active theme for all plots:
#' theme_set(theme_jml())
#' pl_ogtt

theme_jml <- function(
    base_size = 11,
    base_family = "sans",
    labels_color = "black",
    axis_color = "black",
    bg_color = "transparent",
    grid_color = "grey90",
    legend.position = "bottom"
){
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(plot.title = element_text(size = base_size*1.5, face = 2, color = labels_color),
          axis.title.x = element_text(size = base_size*1.25, color = labels_color),
          axis.title.y = element_text(size = base_size*1.25, color = labels_color),
          plot.caption = element_text(size = base_size*0.8, color = labels_color, face = "italic"),
          plot.background = element_rect(color = NA, fill = bg_color),
          legend.position = legend.position,
          legend.background = element_rect(fill = bg_color), 
          panel.background = element_rect(fill = bg_color),
          panel.border = element_blank(),
          axis.line = element_line(color = axis_color),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(color = grid_color, linetype = 2)
    )
}
