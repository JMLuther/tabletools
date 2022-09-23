#' Functions to clean character strings 
#'
#' `txt_clean_nonascii` removes non-standard characters from text, but keeps space, -, and . by default, using regex matching.
#' Default matching can be overridden by `regex` option.
#' 
#' @param string a character string, or character vector
#' @param regex 
#' @param ... other options passed to [base::gsub()]
#'
#' @returns A character string or vector, now stripped of any non-matching values
#' @export
#'
#' @examples
#' test_strings <- c("\"The Erlenmeyer Flask\"‡", "\"My Struggle\"‡", "\"Founder's Mutation\"", "\"Mulder & Scully Meet the Were-Monster\"",
#' "\"Home Again\"", "\"Babylon\"", "\"My Struggle II\"‡" )
#' txt_clean_nonascii(test_strings)

txt_clean_nonascii <- function(string, regex = "[^[:alnum:][:blank:]+?&/\\-\\.]", ...) {
  gsub(regex, "", string)
  }



