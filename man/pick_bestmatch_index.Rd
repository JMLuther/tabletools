% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pick_bestmatch_index.R
\name{pick_bestmatch_index}
\alias{pick_bestmatch_index}
\title{Fuzzy matching functions}
\usage{
pick_bestmatch_index(string_vector, string_tomatch, method = "jaccard")
}
\arguments{
\item{string_vector}{a character vector of possible values}

\item{string_tomatch}{a single character value for which a match is requested}

\item{method}{Method for distance calculation. The default is "jaccard", see \code{\link[stringdist]{stringdist-metrics}}.}
}
\value{
`pick_bestmatch_index` returns the integer index for the best-scoring match
``
`pick_bestmatch_value` returns the character value for the best-scoring match.

`pick_bestmatch_score` returns the calculated score for the best-scoring match.
}
\description{
Functions to match text using fuzzy matching from a vector of possible matches. Bases on `stringdist` functions.
Note: the `stringdist` package is required to be installed to run these functions.
}
\examples{
single_char <- "A Very Specific Title"
possible_matches <- c("shouldnt_match", "extraneous_text", "random_text", "oiphjhdfkl", "very_specific_title")

pick_bestmatch_index(possible_matches, single_char)
pick_bestmatch_value(possible_matches, single_char)
pick_bestmatch_score(possible_matches, single_char)

# comparison of possible methods:
possible_methods = c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")
sapply(possible_methods, function(x) pick_bestmatch_index(possible_matches, single_char, method = x))
sapply(possible_methods, function(x) pick_bestmatch_value(possible_matches, single_char, method = x))
sapply(possible_methods, function(x) pick_bestmatch_score(possible_matches, single_char, method = x))

}
