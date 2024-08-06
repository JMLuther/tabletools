#' @export
#' @rdname convert_length_to_m

convert_height_to_m <- function(height, height_units){
  switch(height_units,
         m = height, meters = height,
         cm = height/100, centimeters = height/100,
         inches = height*0.0254, "in" = height*0.0254, ins = height*0.0254,
         stop("invalid units; use m, cm, or inches"))
}

