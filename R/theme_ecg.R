#' ECG ggplot theme
#' ggplot theme for use in plotting Apple health ECG data in R.
#' @export
#'
theme_ecg <- function() {
  theme_minimal() +
    theme(
      plot.caption = element_text(family = "mono", size = 12, color = "black"),
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      axis.text = element_blank(),
      panel.grid.major.x = element_line(
        color = "red",
        linewidth = 0.25,
        linetype = 1
      ),
      panel.grid.minor.x = element_line(
        color = "salmon",
        linewidth = 0.25,
        linetype = 2
      ),
      panel.grid.major.y = element_line(
        color = "red",
        linewidth = 0.25,
        linetype = 1
      ),
      panel.grid.minor.y = element_line(
        color = "salmon",
        linewidth = 0.25,
        linetype = 2
      )
    )
}
