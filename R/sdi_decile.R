#' Get Social Deprivation Index (SDI) by Zipcode
#'
#' Return the SDI decile calculated by 5-year intervals. SDI decile estimates
#' are available for years ending in 2012, 2015, 2016, 2017, 2018, 2019.
#' Quantiles are calculated by the SDI score for each year. Detailed description
#' and source data from
#' \href{https://www.graham-center.org/maps-data-tools/social-deprivation-index.html}{Robert
#' Graham Center}.
#'
#' @param zipcode Zipcode area
#' @param year Year (default to most recent = 2019)
#'
#' @return Single value representing the SDI Decile (lower is better)
#' @export
#'
#' @examples
#' sdi_decile(37220)
#' sdi_decile(90210)

sdi_decile <- function(zipcode, year=2019){
  df_sdi[df_sdi$ZCTA5_FIPS=={{zipcode}} & df_sdi$year=={{year}}, ][["sdi_decile"]]
}