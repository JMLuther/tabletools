#' @export
#' @rdname convert_length_to_m

convert_length_to_mm <- function(length, length_units){
  switch(length_units,
         mm = length, millimeters = length,
         m = length*1000, meters = length*1000,
         cm = length*10, centimeters = length*10,
         inches = length*2.54*10, "in" = length*2.54*10, ins = length*2.54*10,
         stop("invalid units; use mm, m, cm, or inches"))
}
