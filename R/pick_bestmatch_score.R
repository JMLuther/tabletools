#' Fuzzy matching functions
#'
#' Functions to match text using fuzzy matching from a vector of possible matches. Bases on `stringdist` functions.
#' Note: the `stringdist` package is required to be installed to run these functions.
#' 
#' @inheritParams pick_bestmatch_value
#'
#' @return `pick_bestmatch_index` returns the integer index for the best-scoring match
#' ``
#' `pick_bestmatch_value` returns the character value for the best-scoring match.
#' 
#' `pick_bestmatch_score` returns the calculated score for the best-scoring match.
#' @export
#' @examples
#' single_char <- "A Very Specific Title"
#' possible_matches <- c("shouldnt_match", "extraneous_text", "random_text", "oiphjhdfkl", "very_specific_title")
#' 
#' pick_bestmatch_index(possible_matches, single_char)
#' pick_bestmatch_value(possible_matches, single_char)
#' pick_bestmatch_score(possible_matches, single_char)
#' 
#' # comparison of possible methods:
#' possible_methods = c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")
#' sapply(possible_methods, function(x) pick_bestmatch_index(possible_matches, single_char, method = x))
#' sapply(possible_methods, function(x) pick_bestmatch_value(possible_matches, single_char, method = x))
#' sapply(possible_methods, function(x) pick_bestmatch_score(possible_matches, single_char, method = x))
#' 

pick_bestmatch_score <- function(string_vector, string_tomatch, method = "jaccard", ...) {
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    stop(
      "Package \"stringdist\" must be installed to use this function.",
      call. = FALSE
    )}
  match_score = stringdist::stringdist(string_vector, string_tomatch, method = method)
  best_score = which(match_score == min(match_score))
  return(match_score[best_score])
}
