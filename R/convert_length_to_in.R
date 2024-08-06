#' @export
#' @rdname convert_length_to_m

convert_length_to_in <- function(length, length_units){
  switch(length_units,
         "in" = length, "inches" = length, "ins" = length,
         cm = length/2.54, centimeters = length/2.54,
         m = length/0.0254, "meters"=length/0.0254,
         mm = length/25.4, "millimeters"=length/25.4,
         stop("invalid units; use m, cm, or inches"))
}
