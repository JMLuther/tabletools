#' Read and Plot Apple Health ECG data in R
#'
#' Uses ECG data exported from apple watch ECG, and plots using voltage. Data is converted to standard ECG coordinates internally matching ECG convention (x units = 40ms/box, y units = 0.1mV/box). Data is plotted across 6 rows by default, assuming a 30s tracing, but can be adjusted using the `width_max` option.
#'
#' @param ecg_file ECG data file in csv format
#' @param width_max Desired width per row
#'
#' @returns ECG as a ggplot
#'
#' @export
#' @examples
#' library(ggplot2, data.table)
#' ecg_example_filepath =  system.file("extdata", "ecg_example.csv", package = "tabletools")
#' plot_ecg(ecg_example_filepath)
#' plot_ecg(ecg_example_filepath, width_max = 100)
#' plot_ecg(ecg_example_filepath, width_max = 125)
#' plot_ecg(ecg_example_filepath, width_max = 150)
#' plot_ecg(ecg_example_filepath, width_max = 300)
#'
plot_ecg <- function(ecg_file, width_max = 125) {
  ## ├ ECG voltage ----
  ecg_dt_dat = data.table::fread(
    ecg_file,
    skip = 13,
    col.names = c("voltage", "_")
  )
  ## ├ Metadata ----
  ecg_dt_meta <- data.table::fread(
    ecg_file,
    skip = 0,
    nrows = 10,
    header = F,
    blank.lines.skip = T,
    col.names = c("field", "value")
  )
  ecg_sample_Hz = as.numeric(str_extract(
    ecg_dt_meta$value[ecg_dt_meta$field == "Sample Rate"],
    "\\d+"
  ))
  ecg_date <- ecg_dt_meta$value[ecg_dt_meta$field == "Recorded Date"]
  # rescale values to 1mm plotting grid
  # note: each 25mm grid = 1second (per standard ECG convention)
  ecg_sample_rate_ms = 1000 / ecg_sample_Hz # Hz = 1/s; all appear to be 512 Hz
  ecg_dt_dat$time_ms <- (seq_along(ecg_dt_dat$voltage) - 1) * ecg_sample_rate_ms
  ecg_dt_dat$voltage_mV <- ecg_dt_dat$voltage / 1000 # convert uV to mV
  ecg_dt_dat$x = ecg_dt_dat$time_ms / 40 # 1mm = 40ms (25mm/s "paper speed")
  # note: each 1mm grid = 0.1mV (per standard ECG convention)
  ecg_dt_dat$y = ecg_dt_dat$voltage_mV / 0.1 # 1mm = 0.1mV (single standard ecg)
  ecg_interpretation <- ecg_dt_meta$value[ecg_dt_meta$field == "Classification"]

  # width_max = 125 # 125 = 5 seconds each strip
  ecg_dt_dat$row_n = ecg_dt_dat$x %/% width_max + 1 # row grouping number (for facet)
  ecg_dt_dat$x_sub = ecg_dt_dat$x %% width_max # x variable, start 0 for each row

  ecg_dt_dat |>
    ggplot(aes(x_sub, y)) +
    geom_line(color = "black") +
    facet_wrap(~row_n, ncol = 1) +
    labs(
      x = NULL,
      y = NULL,
      subtitle = "Speed: 25 mm/sec  Limb: 10 mm/mV",
      title = paste0(
        "ECG Date:",
        ecg_date,
        "\nAppleWatch Interpretation: ",
        ecg_interpretation
      )
    ) +
    scale_x_continuous(
      limits = c(0, width_max),
      breaks = seq(0, width_max, by = 5),
      minor_breaks = seq(0, width_max, by = 1)
    ) + # major breaks = 200ms; minor = 40ms
    scale_y_continuous(
      # will probably need to adjust limits based on different ECG voltage range
      breaks = seq(-2.5, 7.5, by = 5),
      minor_breaks = seq(-2.5, 7.5, by = 1)
    ) + # major breaks = 200ms; minor = 40ms
    coord_fixed(ratio = 1) +
    theme_ecg()
}
