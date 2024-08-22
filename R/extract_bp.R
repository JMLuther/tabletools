#' Extract blood pressure values from text (SBP/DBP)
#'
#' Separates SBP and DBP from a string of blood pressure readings, and returns
#' it as a dataframe. Mean arterial pressure is added unless default `meanBP`
#' option changed.
#'
#' @param text A string containing blood pressure readings
#' @param digits Desired number of signficant digits, default=3
#' @param meanBP if FALSE, does not return calculated MAP
#'
#' @return A dataframe containing SBP, DBP and MAP
#' @export
#'
#' @examples
#' bpreadings <- c("120/80", "90/50", "220/40")
#' bpreadings2 <- c("120 / 80", "90 / 50", "220 / 40")
#' extract_bp(bpreadings)
#' extract_bp(bpreadings2)
#' extract_bp(bpreadings2, meanBP = F)

extract_bp <- function(text, meanBP=TRUE, digits=3){
  sbp = as.numeric( gsub("([0-9]+)[ /]+[ 0-9]+", "\\1", text))
  dbp = as.numeric( gsub("[[:alnum:]]+[ /]+([0-9]+)", "\\1", text))

  if (meanBP==FALSE) {
  return(data.frame("sbp"= format(sbp, digits=digits), 
                    "dbp"= format(dbp, digits=digits), 
                    stringsAsFactors = F))}
  else {
      mapbp = calculate_map(sbp, dbp)
      return(data.frame("sbp"= format(sbp, digits=digits), 
                        "dbp"= format(dbp, digits=digits), 
                        "meanbp"= format(mapbp, digits=digits), 
                        stringsAsFactors = F))}
    }
