#' Calculate age from date of birth
#'
#' @param dob Date of birth, preferably "YYYY-MM-DD" format
#' @param date Date to calculate age from, default is today
#' @param date_format defaults to "ymd" "YYYY-MM-DD" format
#' @param units units to return age in, default is "years"; other options are "days", "weeks", "months"
#'
#' @return age in years
#' @export
#'
#' @examples
#' calculate_age(dob = "Jan 5, 1960", date_format = "mdy")
#' calculate_age(dob = "1/5/1960", date_format = "mdy")
#' calculate_age(dob = "1960-01-05", date_format = "ymd")
#' calculate_age(dob = "1960-01-05", date_format = "ymd", units = "days")
#' calculate_age(dob = "1960-01-05", date_format = "ymd", units = "weeks")

calculate_age <- function(dob, date="today", date_format="ymd", units="years") {
  if (date=="today") {
    date = Sys.Date()
  } else if (date_format=="ymd") {
    date = lubridate::ymd(date)
  } else if (date_format=="mdy") {
    date = lubridate::mdy(date)
  }
  
  dob = switch(date_format,
               "mdy" = lubridate::mdy(dob),
               "ymd" = lubridate::ymd(dob))

  age = as.numeric(lubridate::interval(dob, date), {{units}})
  return(age)                 
}
