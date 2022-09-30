#' Summary text functions for html tables
#'
#' @description This function returns html formatted text summary of a Chi-sqauare test. 
#' The input object must be a chisq.test object, and the p-value is formatted by the 
#' `htmlTable::txtPval()` function.  
#' 
#' @param chisquare_object A Chi-square object 
#' @export
#' @seealso \code{\link[htmlTable]{txtPval}}, \code{\link[base]{sprintf}}, \code{\link[stats]{chisq.test}}
#' @examples
#' library(htmlTable)
#' # a basic 2x2 table
#' table(mtcars$vs,mtcars$cyl) # the table
#' cstest1 <-chisq.test(table(mtcars$vs,mtcars$am)) # the chi.square test object
#' htmlTable::htmlTable(txt_chisquare_report(cstest1))
#' 
#' # a 2x3 table
#' table(mtcars$vs,mtcars$cyl)
#' cstest2 <-chisq.test(table(mtcars$vs,mtcars$cyl))
#' htmlTable::htmlTable(txt_chisquare_report(cstest2))


txt_chisquare_report <- 
  function(chisquare_object, ....) {
    sprintf("<i>&Chi;<sup>2</sup></i>(%1.0f, <i>N</i>=%1.0f)=%1.1f, <i>p</i>=%s",
            chisquare_object$parameter,
            sum(chisquare_object$observed),
            chisquare_object$statistic,
            htmlTable::txtPval(chisquare_object$p.value))
  }

