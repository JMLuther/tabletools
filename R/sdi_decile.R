#' Get Social Deprivation Index (SDI) by Zipcode
#'
#' Return the SDI decile calculated by 5-year intervals. SDI decile estimates
#' are based on 2015-2019. Detailed description and source data from
#' \href{https://www.graham-center.org/maps-data-tools/social-deprivation-index.html}{Robert
#' Graham Center}.
#'
#' @param zipcode Zipcode area
#'
#' @return Single value representing the SDI Decile (lower is better)
#' @export
#'
#' @examples
#' sdi_decile(37220)
#' sdi_decile(90210)

sdi_decile <- function(zipcode) {
  df_sdi[df_sdi$ZCTA5_FIPS == {{ zipcode }}, ][["sdi_decile"]]
}
