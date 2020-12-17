#' Modify rcorr results
#' 
#' Modify the Hmisc::rcorr output for nicer printing with labels in a heatmap
#'
#' @description Modifies the `rcorr` output from the `Hmisc::rcorr()` function.
#' The result is an array with additional matrices (`$P_simplified`, `$label`, and `P_lab`).
#' Optionally will replace the variable names with labels from a lookup table (see example)
#' @param rcmat An `rcorr` class object created by the function `Hmisc::rcorr`` 
#' @param lookup_varTable A lookup table structured as a dataframe with variable lables. 
#' This is optional and by default is not used.
#' @return An rcorr array with the following matrices:
#' \itemize{
#'   \item correlation (`$r`). unchanged from the original
#'   \item n (`$n`). unchanged from the original
#'   \item P-value (`$P`). unchanged from the original
#'   \item Simplified P-value (`$P_simplified`). a text matrix with P-values
#'   \item Labeled P-value (`P_lab`). a text matrix with significance of P-values indicated with `*`, `**`, and `***`. Results are unadjusted.
#'   \item label (`$label`). a text matrix with the `r` and `p-value` summary, useful as a hovertext
#'   \item label (`$label_html`). a text matrix with the `r` and `p-value` summary, useful to pass to `htmlTable`
#' }
#' @seealso \code{\link[Hmisc]{rcorr}}, \code{\link[heatmaply]{heatmaply}}
#' @export
#' @examples
#' require(Hmisc)
#' require(tibble)
#' require(heatmaply)
#' require(ggplot2)
#' # create a LOOKUP TABLE
#' lookup_varTable <-
#'   tibble::tribble(~varname, ~labels,
#'           "mpg",	"Miles/(US) gallon",
#'           "cyl",	"Number of cylinders",
#'           "disp",	"Displacement (in.<sup>3</sup>)",
#'           "hp",	  "Gross horsepower",
#'           "drat",	"Rear axle ratio",
#'           "wt",	  "Weight (1000 lbs)",
#'           "qsec",	"1/4 mile time",
#'           "vs",	  "Engine (0 = V-shaped, 1 = straight)",
#'           "am",	  "Transmission (0 = automatic, 1 = manual)",
#'           "gear",	"Number of forward gears",
#'           "carb",	"Number of carburetors"  )
#' test <- Hmisc::rcorr(as.matrix(mtcars) )
#' test_result <- rcorr_clean(test, lookup_varTable = lookup_varTable)
#' 
#' # make an interactive HEATMAP with heatmaply
#' heatmaply::heatmaply(test_result$r,
#'                      custom_hovertext = test_result$label,
#'                      cellnote = test_result$P_lab,
#'                      na.value = "black",
#'                      scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(low = "blue", high = "red", midpoint = 0, limits = c(-1, 1)),
#'                      cellnote_textposition = "bottom center",
#'                      key.title = "Spearman's rho:")


rcorr_clean <- function(rcmat, lookup_varTable = NULL) {
  stopifnot(class(rcmat) == "rcorr")
  rcmat_nrow = nrow(rcmat$r)
  diag(rcmat$P) <- 1 # replace NA diag with 1
  # Simple P-values
  rcmat$P_simplified <-
    apply(rcmat$P, c(1,2), function(x) if(x < 0.0001) {x = sprintf("%.1E", x)
    } else if(x <0.001) {x = sprintf("%.4f", x)
    } else if(x <0.05) {x = sprintf("%.3f", x)
    } else {x = sprintf("%.2f", x)})
  # labels
  rcmat$label <- matrix(mapply(function(x,y) sprintf("Spearman's rho= %.2f\np=%s", x, y), 
                               x=rcmat$r, y=rcmat$P_simplified),
                         nrow = rcmat_nrow,
                         ncol = rcmat_nrow)
  rownames(rcmat$label) <- rownames(rcmat$r)
  colnames(rcmat$label) <- colnames(rcmat$r)
  # n as diagonal
  # HTML-style labels
  rcmat$label_html <- rcmat$P_simplified
  rcmat$label_html[upper.tri(rcmat$label_html)] <- sprintf("<i>P</i>=%s", rcmat$label_html[upper.tri(rcmat$label_html)] )
  rcmat$label_html[lower.tri(rcmat$label_html)] <- sprintf("<i>&rho;</i>=%.2f", rcmat$r[lower.tri(rcmat$r)])
  diag(rcmat$label_html) <- sprintf("<strong>n=%1.0f</strong>", diag(rcmat$n)) # this is a vector, not a matrix
  # text format p-values
  rcmat$P_lab <-
    apply(rcmat$P, c(1,2), function(x) if(x < 0.001) {x = "***"
    } else if(x <0.01) {x = "**"
    } else if(x <0.05) {x = "*"
    } else {x = ""})
  diag(rcmat$P_lab) <- sprintf("%1.0f", diag(rcmat$n)) # this is a vector, not a matrix
  # LOOKUPTABLE
  if(is.null(lookup_varTable)) {
    return(rcmat)} else {
      rownames(rcmat$r) = lookup_varTable$labels[match(rownames(rcmat$r), lookup_varTable$varname)]
      colnames(rcmat$r) = lookup_varTable$labels[match(colnames(rcmat$r), lookup_varTable$varname)]
      return(rcmat)}
  return(rcmat)
}

# TO DO: apply multiple-testing/FDR correction to p-values

