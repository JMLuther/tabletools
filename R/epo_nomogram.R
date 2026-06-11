#' Erythropoietin (EPO) Nomogram Plot
#' Plots a single matched hematocrit, EPO data pair over the historical EPO nomogram. Data was taken from \href{https://pubmed.ncbi.nlm.nih.gov/2017231/}{Erslev. NEJM. 1991}
#'
#' @param hematocrit Hematocrit (%) that corresponds to EPO
#' @param epo Serum Erythropoietin (mU/mL)
#'
#' @returns a ggplot of datapoint over the EPO nomogram plot
#'
#' @export
#' @examples
#' epo_nomogram(35, 12)
#' epo_nomogram(29, 14)
#' epo_nomogram(31.7, 13)

epo_nomogram <- function(hematocrit, epo) {
  theme_set(tabletools::theme_jml())
  plot_epo = tabletools:::plot_epo
  plot_epo +
    annotate("point", x = hematocrit, y = epo, colour = "red", size = 4) +
    annotate(
      "text",
      x = hematocrit,
      y = epo - 2,
      label = paste("[", hematocrit, ", ", epo, "]"),
      color = "red",
      vjust = 1
    ) +
    labs(
      title = "Expected Erythropoietin Concentrations in Patients\nwith uncomplicated anemia"
    )
}
