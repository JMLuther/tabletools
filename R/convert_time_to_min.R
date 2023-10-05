#' Convert time to minutes
#'
#' @param time numeric vector
#' @param time_units specify the units of current time values  
#'
#' @return numeric vector 
#' @export
#'
#' @examples
#' convert_time_to_min(1.5, "hr")
#' convert_time_to_min(1.5, "hrs")
#' convert_time_to_min(1.5, "hours")
#' convert_time_to_min(60, "secs")
#' convert_time_to_min(60, "s")

convert_time_to_min <- function(time, time_units){
  switch(time_units,
         s= time/60, secs= time/60, seconds= time/60,  
         "min."= time, min= time,  minutes = time, 
         hr= time*60, hrs= time*60, hours = time*60,
         stop("invalid units; use s/seconds, min/minutes or hr/hrs/hours"))
}
